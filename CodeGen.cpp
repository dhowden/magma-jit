// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <iostream>
#include <sstream>
#include <ctime>
#include <cstdio>

#include "CodeGen.hpp"
#include "ContextLookAhead.hpp"

#include "Parser.hpp"

using namespace llvm;

extern cl::opt<bool> EmitLLVM;
extern cl::opt<bool> EmitLLVMOpt;
extern cl::opt<bool> EnabledTailEliminationPass;
extern cl::opt<bool> SkipVerify;

//===----------------------------------------------------------------------===//
// C Externed Support Functions (used in JIT)
//===----------------------------------------------------------------------===//

Value* CodeGen::createStringPtr(Value* String)
{
   Value* StringGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false)
   };
   return Builder->CreateInBoundsGEP(String, makeArrayRef(StringGEPIdx));
}

void CodeGen::createPrintfCall(Value* Format, std::vector<Value *> &Args)
{
   std::string* symbolName;
   Function* printfFunction;

   /// Check if we have already declared this function, and it not do it now
   if ((symbolName = context.getInternalSymbol("printf"))) {
      printfFunction = module->getFunction(*symbolName);
   } else {
      Type* args[] = { Type::getInt8PtrTy(getGlobalContext()) };
      FunctionType* printfFunctionType =
         FunctionType::get(Type::getInt32Ty(getGlobalContext()), makeArrayRef(args), true);
      printfFunction = Function::Create(printfFunctionType, Function::ExternalLinkage, "printf", module);
      printfFunction->setCallingConv(CallingConv::C);

      /// Register as internal symbol
      context.registerInternalSymbol("printf", new std::string(printfFunction->getName()));
   }

   /// Construct the call
   std::vector<Value *> CallArgs(1+Args.size());
   CallArgs[0] = Format;
   for (int i = 0; i < Args.size(); i++) CallArgs[i+1] = Args[i];
   Builder->CreateCall(printfFunction, CallArgs);
}

/// for TimeStatement construct (a lot of a hack :-/)
std::stack<clock_t> stack;

extern "C"
void timer_start()
{
   stack.push(clock());
}

extern "C"
void timer_stop()
{
   clock_t stop = clock();
   clock_t start = stack.top();
   stack.pop();
   double millis = ((double)(stop - start)) / CLOCKS_PER_SEC;
   std::printf("Time: %.4f\n", millis);
}

/// for AssertStatement construct
extern "C"
void print_assert_error(unsigned long d)
{
   std::fprintf(stderr, "Assertion Failed: Line %ld\n", d);
   exit(1);
}

//===----------------------------------------------------------------------===//
// Root CodeGen methods
//===----------------------------------------------------------------------===//

CodeGen::CodeGen() {}
CodeGen::~CodeGen() {}

void CodeGen::generateCode(BlockAST &block)
{
   module = new Module("main", getGlobalContext());
   Builder = new IRBuilder<>(getGlobalContext());

   std::vector<Type *> argTypes;
   FunctionType *fType = FunctionType::get(Type::getVoidTy(getGlobalContext()),
      makeArrayRef(argTypes), false);
   mainFunction = Function::Create(fType, GlobalValue::InternalLinkage, "main",
      module);

   BasicBlock *mainEntryBB = BasicBlock::Create(getGlobalContext(), "entry",
      mainFunction, 0);
   Builder->SetInsertPoint(mainEntryBB);

   context.createScope(mainEntryBB);
   block.accept(*this);

   Builder->CreateRet(0);
   context.popScope();

   /* useful to have this pre-opt IR output before verify is called */
   if (EmitLLVM) {
      PassManager TheEmitLLVMPassManager;
      TheEmitLLVMPassManager.add(createPrintModulePass(outs()));
      TheEmitLLVMPassManager.run(*module);
   }

   /* check the code for consistency and what-not! */
   if (!SkipVerify)
      verifyFunction(*mainFunction);

   PassManager PM;
   PM.add(createBasicAliasAnalysisPass());
   PM.add(createPromoteMemoryToRegisterPass());
   PM.add(createInstructionCombiningPass());
   PM.add(createReassociatePass());
   PM.add(createGVNPass());
   PM.add(createCFGSimplificationPass());

   if (EnabledTailEliminationPass)
      PM.add(createTailCallEliminationPass());

   if (EmitLLVMOpt)
      PM.add(createPrintModulePass(outs()));

   PM.run(*module);
}

GenericValue CodeGen::runCode()
{
   InitializeNativeTarget();

   std::string ErrStr;
   ExecutionEngine *executionEngine =
      EngineBuilder(module).setErrorStr(&ErrStr).setOptLevel(CodeGenOpt::Aggressive).create();
   if (!executionEngine) {
      std::stringstream str;
      str << "Could not create ExecutionEngine: " << ErrStr << std::endl;
      Error::exit(str.str());
   }

   std::string *symbolName;
   Function *function;

   if ((symbolName = context.getInternalSymbol("timer_start"))) {
      function = module->getFunction(*symbolName);
      executionEngine->addGlobalMapping(function, (void *)&timer_start);

      if ((symbolName = context.getInternalSymbol("timer_stop"))) {
         function = module->getFunction(*symbolName);
         executionEngine->addGlobalMapping(function, (void *)&timer_start);
      } else {
         Error::exit("timer_start is defined but timer_stop isn't");
      }
   }

   if ((symbolName = context.getInternalSymbol("print_assert_error"))) {
      function = module->getFunction(*symbolName);
      executionEngine->addGlobalMapping(function, (void *)&print_assert_error);
   }

   std::vector<GenericValue> noargs;
   GenericValue v = executionEngine->runFunction(mainFunction, noargs);
   return v;
}

//===----------------------------------------------------------------------===//
// Alloca creation methods
//===----------------------------------------------------------------------===//

Value* CodeGen::createLocal(const std::string &name, Type* type)
{
   return createLocalInScope(name, type, context.currentScope());
}

Value* CodeGen::createLocalInScope(const std::string &name, Type* type, Scope* scope)
{
   Value *alloca = createAllocaInBasicBlock(scope->EntryBB, type, name);
   scope->addLocal(name, alloca);
   return alloca;
}

Value* CodeGen::createAllocaInBasicBlock(BasicBlock* EntryBB, Type* type, const std::string &name)
{
   IRBuilder<> TmpB(EntryBB, EntryBB->begin());
   return TmpB.CreateAlloca(type, 0, name.c_str());
}

Value* CodeGen::getOrCreateLocalForAssignment(const std::string &name, Type *type, NodeAST &node)
{
   /// See if there is a scope which already has a variable of this name
   /// that we can assign to
   Scope *scope = context.getScopeContainingLocalForAssignment(name);
   if (scope) {
      /// check that we can assign to this name (in this scope)
      if (scope->canAssignLocal(name)) {
         Value *Alloca = scope->getLocal(name);
         if (PointerType *PT = dyn_cast<PointerType>(Alloca->getType())) {
            if (PT->getElementType() == type) {
               return Alloca;
            }
            if (context.getLoopScope() != NULL) {
               Error::exit(node, "attempting to change type of variable '" + name + "'"
                  + " from '" + MagmaType::getTypeName(PT->getElementType())
                  + "' to '" + MagmaType::getTypeName(type) + "' from within loop scope");
            }
            /// Create a new alloca to override the old one...
            return createLocalInScope(name, type, scope);
         }
         Error::exit(node, "Could not resolve type of registered alloca!");
      } else {
         Error::exit(node, "variable '" + name + "' cannot be overwritten");
      }
   }

   scope = context.getScopeForCreatingLocalAssignment(name);
   if (!scope)
      Error::exit("could not find scope to assign variable too - internal error :-(");

   return createLocalInScope(name, type, scope);
}

Value* CodeGen::getOrCreateLocalForAssignment(const std::string &name, Type *type, NodeAST *node)
{
   return getOrCreateLocalForAssignment(name, type, *(node));
}

//===----------------------------------------------------------------------===//
// Visitor Pattern methods
//===----------------------------------------------------------------------===//

void CodeGen::visit(const BlockAST &block)
{
   StatementList::const_iterator it;
   for (it = block.statements.begin(); it != block.statements.end(); ++it)
      (**it).accept(*this);
}

void CodeGen::visit(const IntegerAST &integer)
{
   lastValue = ConstantInt::get(MagmaType::getIntegerTy(), integer.Value, true);
}

void CodeGen::visit(const BooleanAST &boolean)
{
   lastValue = ConstantInt::get(MagmaType::getLogicalTy(), boolean.Value, true);
}

void CodeGen::visit(const SequenceAST &sequence)
{
   ExpressionList::const_iterator it;

   Type* elementType;
   Value *firstValue = NULL;

   /// Get the type of the first element, if there is one!
   if (sequence.Expressions->size()) {
      it = sequence.Expressions->begin();
      (*it)->accept(*this);
      firstValue = lastValue;
      elementType = firstValue->getType();
   } else {
      elementType = MagmaType::getIntegerTy();
   }

   Value *SequenceSize = ConstantInt::get(MagmaType::getIntegerTy(), sequence.Expressions->size(), true);
   Value *Result = MagmaType::createSequence(module, Builder, elementType, SequenceSize);

   Value *eltPointer;
   std::vector<Value *> arrayGEPIdx(3);
   arrayGEPIdx[0] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, true);
   arrayGEPIdx[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1, true);

   /// We visited the first value to determine the type of the sequence...
   if (firstValue) {
      int i = 0;
      arrayGEPIdx[2] = ConstantInt::get(MagmaType::getIntegerTy(), i++, true);
      eltPointer = Builder->CreateInBoundsGEP(Result, makeArrayRef(arrayGEPIdx));
      Builder->CreateStore(firstValue, eltPointer);

      for (it = ++sequence.Expressions->begin(); it != sequence.Expressions->end(); ++it) {
         arrayGEPIdx[2] = ConstantInt::get(MagmaType::getIntegerTy(), i++, true);
         eltPointer = Builder->CreateInBoundsGEP(Result, makeArrayRef(arrayGEPIdx));
         (*it)->accept(*this);

         if (!MagmaType::typesAreEqual(lastValue->getType(), elementType)) {
            std::stringstream str;
            str << "cannot put '" << MagmaType::getTypeName(lastValue->getType())
               << "' into a '" << MagmaType::getTypeName(Result) << "'";
            Error::exit((*it), str.str());
         }
         Builder->CreateStore(lastValue, eltPointer);
      }
   }
   lastValue = Result;
}

void CodeGen::visit(const TupleAST &tuple)
{
   ExpressionList::const_iterator it;

   std::vector<Value *> elementValues(tuple.Expressions->size());
   std::vector<Type *> elementTypes(tuple.Expressions->size());
   unsigned int i;
   for (i = 0, it = tuple.Expressions->begin(); i < tuple.Expressions->size(); ++i, ++it) {
      (*it)->accept(*this);
      elementValues[i] = lastValue;
      elementTypes[i] = lastValue->getType();
   }

   Value *Result = MagmaType::createTuple(module, Builder, elementTypes);

   Value *eltPointer;
   std::vector<Value *> tupleElementGEPIdx(2);
   tupleElementGEPIdx[0] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, true);

   for (i = 0; i < elementValues.size(); ++i) {
      tupleElementGEPIdx[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), i + 1, true);
      eltPointer = Builder->CreateInBoundsGEP(Result, makeArrayRef(tupleElementGEPIdx));
      Builder->CreateStore(elementValues[i], eltPointer);
   }
   lastValue = Result;
}

void CodeGen::visit(const IndexLookupAST &index_lookup)
{
   Value *local;
   index_lookup.Expression->accept(*this);
   local = lastValue;

   Value *indexValue;
   index_lookup.IndexExpression->accept(*this);
   indexValue = lastValue;
   if (!MagmaType::isIntegerTy(indexValue))
      Error::exit(index_lookup.IndexExpression, "index lookups must have index type of " +
         MagmaType::getTypeName(MagmaType::getIntegerTy()) + " not '" +
         MagmaType::getTypeName(indexValue));

   /// To be indexable, we need either a seq or a tup
   if (MagmaType::isSequenceTy(local->getType())) {
      lastValue = MagmaType::getSequenceElement(module, Builder, local, indexValue);
      return;
   }

   if (MagmaType::isTupleTy(local->getType())) {
      lastValue = MagmaType::getTupleElement(module, Builder, local, indexValue);
      return;
   }

   Error::exit(index_lookup.IndexExpression,
      "index lookups not supported on objects of type " + MagmaType::getTypeName(local->getType()));
}

void CodeGen::visit(const IdentifierAST &identifier)
{
   Value *local = context.getLocalForLoad(identifier.Name);
   if (!local)
      Error::exit(identifier, "undeclared variable '" + identifier.Name + "'");

   /// FIXME: This needs to be limited to procedure calls!!
   if (identifier.IsReference)
      lastValue = local;
   else
      lastValue = Builder->CreateLoad(local, identifier.Name.c_str());
}

void CodeGen::visit(const TypeAST &type)
{
   std::vector<Type *> elementTypes;
   if (type.ExtendedTypeList) {
      TypeList::const_iterator it;
      for (it = type.ExtendedTypeList->begin(); it != type.ExtendedTypeList->end(); ++it) {
         (*it)->accept(*this);
         elementTypes.push_back(lastType);
      }
   }

   lastType = MagmaType::constructMagmaType(type.Name, elementTypes);
   if (!lastType)
      Error::exit(type, "invalid type");
}

void CodeGen::visit(const BinaryExpressionAST &binary_expression)
{
   Value *L, *R;

   if (binary_expression.LHS == NULL) {
      binary_expression.RHS->accept(*this);
      R = lastValue;

      if (binary_expression.Op == TNOT) {
         if (!MagmaType::isLogicalTy(R))
            Error::exit(binary_expression, "expected logical type for operator 'not'");
         lastValue = Builder->CreateNot(R, "tmpnot");
         return;
      }

      if (binary_expression.Op == THASH) {
         if (MagmaType::isSequenceTy(R)) {
            lastValue = MagmaType::getSequenceSize(module, Builder, R);
            return;
         }
         if (MagmaType::isTupleTy(R)) {
            lastValue = MagmaType::getTupleSize(module, Builder, R);
            return;
         }

         Error::exit(binary_expression, "# cannot be applied to objects of type '" +
            MagmaType::getTypeName(R) + "'");
      }

      std::stringstream str;
      str << "Requested unary instruction " << binary_expression.Op
         << " has not been implemented yet.";
      Error::exit(str.str());
   } else {
      binary_expression.LHS->accept(*this);
      L = lastValue;
      binary_expression.RHS->accept(*this);
      R = lastValue;

      Instruction::BinaryOps instr = (Instruction::BinaryOps)0;

      switch (binary_expression.Op) {
         case TAND:   instr = Instruction::And;    break;
         case TOR:    instr = Instruction::Or;     break;
         case TXOR:   instr = Instruction::Xor;    break;
         default:                                  break;
      }

      if (instr != 0) {
         if (!MagmaType::isLogicalTy(L) || !MagmaType::isLogicalTy(R))
            Error::exit(binary_expression, "logical parameter expected for this operation");
         lastValue = Builder->CreateBinOp(instr, L, R);
         return;
      }

      switch (binary_expression.Op) {
         case TCEQ:   lastValue = Builder->CreateICmpEQ(L, R, "eqtmp");  return;
         case TCNE:   lastValue = Builder->CreateICmpNE(L, R, "netmp");  return;
         default: break;
      }

      /// Remaining binary ops are for integers
      if (!MagmaType::isIntegerTy(L) || !MagmaType::isIntegerTy(R)) {
         std::stringstream str;
         str << "Binary operation " << binary_expression.Op << " requires integer parameters" << std::endl;
         str << "Key:" << std::endl;
         str << "TCLT " << TCLT << std::endl;
         str << "TCLE " << TCLE << std::endl;
         str << "TCGT " << TCGT << std::endl;
         str << "TCGE " << TCGE << std::endl;
         str << "TCEQ " << TCEQ << std::endl;
         str << "TCNE " << TCNE << std::endl;
         str << "TPLUS " << TPLUS << std::endl;
         str << "TMINUS " << TMINUS << std::endl;
         str << "TMUL " << TMUL << std::endl;
         str << "TDIV " << TDIV << std::endl;
         str << "TMOD " << TMOD << std::endl;
         Error::exit(binary_expression, str.str());
      }

      switch (binary_expression.Op) {
         case TCLT:   lastValue = Builder->CreateICmpULT(L, R, "lttmp"); return;
         case TCLE:   lastValue = Builder->CreateICmpULE(L, R, "letmp"); return;
         case TCGT:   lastValue = Builder->CreateICmpUGT(L, R, "gttmp"); return;
         case TCGE:   lastValue = Builder->CreateICmpUGE(L, R, "getmp"); return;
         case TPLUS:  lastValue = Builder->CreateAdd(L, R, "addtmp");    return;
         case TMINUS: lastValue = Builder->CreateSub(L, R, "subtmp");    return;
         case TMUL:   lastValue = Builder->CreateMul(L, R, "multmp");    return;
         case TDIV:   lastValue = Builder->CreateUDiv(L, R, "divtmp");   return;
         case TMOD:   lastValue = Builder->CreateURem(L, R, "modtmp");   return;
         default: break;
      }

      if (binary_expression.Op == THAT)
         Error::exit(binary_expression, "'^' operator not implemented yet");

      std::stringstream str;
      str << "Requested binary instruction " << binary_expression.Op
         << " has not been implemented yet.";
      Error::exit(str.str());
   }
}

void CodeGen::visit(const AssignmentStatementAST &assignment_statement)
{
   assignment_statement.Expression->accept(*this);
   Value *ExprV = lastValue;

   if (assignment_statement.Index == NULL) {
      /// Check that we don't already have an alloca for the expression...
      Value *local = getOrCreateLocalForAssignment(
         assignment_statement.Identifier->Name, lastValue->getType(), assignment_statement.Identifier);

      lastValue = Builder->CreateStore(lastValue, local);
      return;
   }

   Value *local = context.getLocalForLoad(assignment_statement.Identifier->Name);
   if (!local)
      Error::exit(assignment_statement.Identifier, "undefined variable '" + assignment_statement.Identifier->Name + "'");

   /// To be indexable, we need local to be a **StructType here
   if (PointerType *P1 = dyn_cast<PointerType>(local->getType())) {
      if (MagmaType::isSequenceTy(P1->getElementType())) {
         Value *Load = Builder->CreateLoad(local);

         /// Look at the index value provided
         assignment_statement.Index->accept(*this);
         if (!MagmaType::isIntegerTy(lastValue)) {
            std::stringstream str;
            str << "Index parameters must be of type "
               << MagmaType::getTypeName(MagmaType::getIntegerTy()) << " not "
               << MagmaType::getTypeName(lastValue);
            Error::exit(str.str());
         }
         Value *requiredSize = lastValue;

         /// Resize the sequence (if necessary)
         MagmaType::resizeSequence(module, Builder, local, Load, requiredSize);

         /// Reload the sequence just incase it's been realloced
         Value* potentialNewSequence = Builder->CreateLoad(local, "potential_newsequence");

         Value *eltPointer = MagmaType::getSequenceElementPtr(module, Builder, potentialNewSequence, requiredSize);

         Builder->CreateStore(ExprV, eltPointer);
         return;
      }
      if (MagmaType::isTupleTy(P1->getElementType())) {
         Value *Load = Builder->CreateLoad(local);
         assignment_statement.Index->accept(*this);
         if (!MagmaType::isIntegerTy(lastValue)) {
            std::stringstream str;
            str << "Index parameters must be of type "
               << MagmaType::getTypeName(MagmaType::getIntegerTy()) << " not "
               << MagmaType::getTypeName(lastValue);
            Error::exit(str.str());
         }
         Value *eltPointer = MagmaType::getTupleElementPtr(module, Builder, Load, lastValue);
         Builder->CreateStore(ExprV, eltPointer);
         return;
      }

      Error::exit("index assignment not supported for objects of type " +
         MagmaType::getTypeName(P1->getElementType()));
   }
   Error::exit("Something is really wrong.  The load instruction returning something non-pointerish!");
}

void CodeGen::visit(const StringExpressionAST &string_expression)
{
   lastValue = createConstantStringGlobal(string_expression.Value);
}

Value* CodeGen::createConstantStringGlobal(std::string value, std::string name/* = NULL*/)
{
   Value *globalVariable = context.getInternalStringConstant(value);
   if (!globalVariable) {
      Constant *stringArray = ConstantDataArray::getString(getGlobalContext(), value);
      globalVariable = new GlobalVariable(*module, stringArray->getType(), true,
         GlobalValue::InternalLinkage, stringArray, name);
      context.registerInternalStringConstant(value, globalVariable);
   }
   return createStringPtr(globalVariable);
}

void CodeGen::printBoolElt(Value *Val)
{
   std::string* symbolName;
   Function* printFunction;

   /// Check if we have already defined this function, and if not then create it
   if ((symbolName = context.getInternalSymbol("printBoolElt"))) {
      printFunction = module->getFunction(*symbolName);
   } else {
      /// Store the current insert point (restored at the end)
      BasicBlock *CurrentInsertPointBB = Builder->GetInsertBlock();

      /// Create the function prototype
      Type* boolTy[] = { MagmaType::getLogicalTy() };
      FunctionType* voidFunctionType =
         FunctionType::get(Type::getVoidTy(getGlobalContext()), makeArrayRef(boolTy), false);
      printFunction = Function::Create(voidFunctionType, Function::InternalLinkage, "printBoolElt", module);

      /// Register as internal symbol
      context.registerInternalSymbol("printBoolElt", new std::string(printFunction->getName()));

      /// Create the function basic blocks
      BasicBlock* EntryBB = BasicBlock::Create(getGlobalContext(), "entry", printFunction, 0);
      BasicBlock* TrueBB = BasicBlock::Create(getGlobalContext(), "true", printFunction, 0);
      BasicBlock* FalseBB = BasicBlock::Create(getGlobalContext(), "false", printFunction, 0);

      /// Branch on the parameter value
      Builder->SetInsertPoint(EntryBB);
      Function::arg_iterator AI = printFunction->arg_begin();
      Builder->CreateCondBr(AI, TrueBB, FalseBB);

      /// Print true!
      Builder->SetInsertPoint(TrueBB);
      printValue(createConstantStringGlobal("true"));
      Builder->CreateRetVoid();

      /// Print false!
      Builder->SetInsertPoint(FalseBB);
      printValue(createConstantStringGlobal("false"));
      Builder->CreateRetVoid();

      /// Restore the previous insertion point
      Builder->SetInsertPoint(CurrentInsertPointBB);
   }

   // Call the print function
   Builder->CreateCall(printFunction, Val);
}

void CodeGen::printValue(Value* Val)
{
   bool invalidPrintType = true;
   std::string *symbolName;
   Function *printFunction;

   if (MagmaType::isIntegerTy(Val)) {
      std::vector<Value*> Args(1, Val);
      createPrintfCall(createConstantStringGlobal("%ld"), Args);
      invalidPrintType = false;
   } else if (MagmaType::isLogicalTy(Val)) {
      /// This uses a JIT'ed function which might need to be constructed...
      printBoolElt(Val);
      invalidPrintType = false;
   } else if (MagmaType::isStringTy(Val)) {
      std::vector<Value *> Args(0);
      createPrintfCall(Val, Args);
      invalidPrintType = false;
   } else if (MagmaType::isSequenceTy(Val)) {
      /// Need to rewrite:
      /// x := [1,2,3,...]; print x;
      /// print "["; for i in [1..#x] do print x; if i ne #x then print ","; end if; end for; print "]";

      /// Code here is based on the IterativeStatementAST CodeGen
      Value* SeqSize = MagmaType::getSequenceSize(module, Builder, Val);

      /// From iterative loop scope...
      Function *TheFunction = Builder->GetInsertBlock()->getParent();

      /// Create the "after" block here for possible break
      BasicBlock *AfterBB = BasicBlock::Create(getGlobalContext(), "afterloop", TheFunction);

      std::string iterativeVar = "print_seq";
      context.createIterativeLoopScope(iterativeVar,
         Builder->GetInsertBlock(), AfterBB);
      Value *alloca = createLocal(iterativeVar, MagmaType::getIntegerTy());

      /// Start index
      Builder->CreateStore(ConstantInt::get(MagmaType::getIntegerTy(), 1, false), alloca);

      printValue(createConstantStringGlobal("["));

      BasicBlock *BeforeLoopBB = BasicBlock::Create(getGlobalContext(), "beforeloop", TheFunction);
      Builder->CreateBr(BeforeLoopBB);
      Builder->SetInsertPoint(BeforeLoopBB);

      Value *curIndex = Builder->CreateLoad(alloca, iterativeVar);
      Value *EndCond = Builder->CreateICmpUGE(SeqSize, curIndex, "loopcond");

      BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
      Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

      Builder->SetInsertPoint(LoopBB);

      /// Get the current index
      curIndex = Builder->CreateLoad(alloca, iterativeVar);

      /// Get the element
      Value* Element = MagmaType::getSequenceElement(module, Builder, Val, curIndex);

      /// Print it
      printValue(Element);

      /// Increment position
      Value *nextIndex = Builder->CreateAdd(curIndex,
         ConstantInt::get(MagmaType::getIntegerTy(), 1, false), "nextvar");
      Builder->CreateStore(nextIndex, alloca);

      EndCond = Builder->CreateICmpUGE(SeqSize, nextIndex, "beforeloopbackcond");

      BasicBlock *BeforeLoopBackBB = BasicBlock::Create(getGlobalContext(), "beforeloopback", TheFunction);
      Builder->CreateCondBr(EndCond, BeforeLoopBackBB, AfterBB);

      Builder->SetInsertPoint(BeforeLoopBackBB);
      printValue(createConstantStringGlobal(","));

      Builder->CreateBr(LoopBB);

      Builder->SetInsertPoint(AfterBB);

      printValue(createConstantStringGlobal("]"));

      context.popScope();
      invalidPrintType = false;
   } else if (MagmaType::isTupleTy(Val)) {
      /// Need to rewrite:
      /// x := <1,2,3,...>; print x;
      /// print "<"; for i in [1..#x] do print x; if i ne #x then print ","; end if; end for; print ">";
      /// Sadly only works for constant index lookups at present due to data
      /// structure (struct doesn't allow non-const position lookups).

      /// Code here is based on the IterativeStatementAST CodeGen
      Value* TupSize = MagmaType::getTupleSize(module, Builder, Val);

      /// From iterative loop scope...
      Function *TheFunction = Builder->GetInsertBlock()->getParent();

      /// Create the "after" block here for possible break
      BasicBlock *AfterBB = BasicBlock::Create(getGlobalContext(), "afterloop", TheFunction);

      std::string iterativeVar = "print_tup";
      context.createIterativeLoopScope(iterativeVar,
         Builder->GetInsertBlock(), AfterBB);
      Type *t = Type::getInt32Ty(getGlobalContext());
      Value *alloca = createLocal(iterativeVar, /*MagmaType::getIntegerTy()*/t);

      /* Compute and store the start value */
      Builder->CreateStore(ConstantInt::get(/*MagmaType::getIntegerTy()*/t, 1, false), alloca);

      printValue(createConstantStringGlobal("<"));

      BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
      Builder->CreateBr(LoopBB);

      Builder->SetInsertPoint(LoopBB);
      Value *curValue = Builder->CreateLoad(alloca, iterativeVar);

      /// Get the element
      Value* Element = MagmaType::getTupleElement(module, Builder, Val, curValue);

      /// Print it
      printValue(Element);

      Builder->CreateBr(AfterBB);

      if (!Builder->GetInsertBlock()->getTerminator()) {
         Value *nextValue = Builder->CreateAdd(curValue,
            ConstantInt::get(/*MagmaType::getIntegerTy()*/t, 1, false), "nextvar");
         Builder->CreateStore(nextValue, alloca);

         BasicBlock *BeforeLoopBack = BasicBlock::Create(getGlobalContext(), "beforeloopback", TheFunction);

         Value* EndCond = Builder->CreateICmpUGE(TupSize, nextValue, "loopcond");
         Builder->CreateCondBr(EndCond, BeforeLoopBack, AfterBB);

         Builder->SetInsertPoint(BeforeLoopBack);
         printValue(createConstantStringGlobal(","));

         Builder->CreateBr(LoopBB);
      }

      Builder->SetInsertPoint(AfterBB);

      printValue(createConstantStringGlobal(">"));

      context.popScope();
      invalidPrintType = false;
   }

   if (invalidPrintType)
      Error::exit("Printing is not yet supported for objects of type " +
         MagmaType::getTypeName(Val));
}

void CodeGen::visit(const PrintStatementAST &print_statement)
{
   if (!print_statement.Expressions->size())
      Error::exit(print_statement, "bad syntax - must provide something to print (MAGMA compatibility!)");

   ExpressionList::const_iterator it = print_statement.Expressions->begin();
   while (true) {
      (*it)->accept(*this);
      printValue(lastValue);

      if (++it == print_statement.Expressions->end())
         break;

      printValue(createConstantStringGlobal(" "));
   }

   printValue(createConstantStringGlobal("\n"));
}

void CodeGen::visit(const PrintfStatementAST &printf_statement)
{
   /// Get the actual format string
   std::string &format = printf_statement.StringExpression->Value;
   std::string chunk;
   int count = 0;

   std::string::iterator it = format.begin();
   ExpressionList::const_iterator EL_it = printf_statement.Expressions->begin();
   while (it != format.end()) {
      if (*it == '%') {
         /// Move to the next char, check that we can!
         if (++it == format.end())
            Error::exit(printf_statement.StringExpression, "invalid format string - bad percent character in format string");

         if (*it == '%') {
            /// We've read %%, which really means %, but we need %% for printf
            chunk += "%%";
         } else if (*it == 'o') {
            /// We should output the chunk, if not empty...
            if (!chunk.empty()) {
               printValue(createConstantStringGlobal(chunk));
               chunk.clear();
            }

            /// Check that there is a corresponding expression to print
            if (EL_it == printf_statement.Expressions->end())
               Error::exit(printf_statement.StringExpression, "invalid format string - not enough values to be printed");

            /// Print the value!
            (*EL_it)->accept(*this);
            printValue(lastValue);
            EL_it++;
         } else {
            std::stringstream str;
            str << "invalid format string - bad percent character '" << (*it)
                << "' in format string";
            Error::exit(printf_statement.StringExpression, str.str());
         }
      } else {
         chunk += *it;
      }
      it++;
   }

   /// Output any remaining characters
   if (!chunk.empty())
      printValue(createConstantStringGlobal(chunk));

   /// Check that we have output all of the expressions
   if (EL_it != printf_statement.Expressions->end())
      Error::exit(printf_statement.StringExpression, "invalid format string - more values than placements");
}

void CodeGen::visit(const TimeStatementAST &time_statement)
{
   Function *startTimer = module->getFunction("timer_start");
   if (startTimer == NULL) {
      std::vector<Type *> NoArgs;
      FunctionType *voidFunctionType = FunctionType::get(Type::getVoidTy(getGlobalContext()), false);
      startTimer = Function::Create(voidFunctionType, Function::ExternalLinkage, "timer_start", module);

      if (startTimer == NULL)
         Error::exit("Could not install function timer_start... :-(");
   }

   Builder->CreateCall(startTimer);

   time_statement.Statement->accept(*this);

   Function *stopTimer = module->getFunction("timer_stop");
   if (stopTimer == NULL) {
      std::vector<Type *> NoArgs;
      FunctionType *voidFunctionType = FunctionType::get(Type::getVoidTy(getGlobalContext()), false);
      stopTimer = Function::Create(voidFunctionType, Function::ExternalLinkage, "timer_stop", module);

      if (stopTimer == NULL)
         Error::exit("Could not install function timer_stop... :-(");
   }

   Builder->CreateCall(stopTimer);
}

void CodeGen::visit(const AssertStatementAST &assert_statement)
{
   assert_statement.Expression->accept(*this);

   Function *TheFunction = Builder->GetInsertBlock()->getParent();
   BasicBlock *CallBB = BasicBlock::Create(getGlobalContext(), "callassert",
      TheFunction);
   BasicBlock *ContinueBB = BasicBlock::Create(getGlobalContext(), "continue");

   Builder->CreateCondBr(lastValue, ContinueBB, CallBB);

   Builder->SetInsertPoint(CallBB);

   Function *printAssertError = module->getFunction("print_assert_error");
   if (printAssertError == NULL) {
      std::vector<Type *> IntegerTy1(1, MagmaType::getIntegerTy());
      FunctionType *voidFunctionType =
         FunctionType::get(Type::getVoidTy(getGlobalContext()), IntegerTy1, false);
      printAssertError = Function::Create(voidFunctionType, Function::ExternalLinkage, "print_assert_error", module);
   }

   Builder->CreateCall(printAssertError, ConstantInt::get(MagmaType::getIntegerTy(),
      assert_statement.LineNumber, true));
   /* block must end with terminator instruction */
   Builder->CreateUnreachable();

   TheFunction->getBasicBlockList().push_back(ContinueBB);
   Builder->SetInsertPoint(ContinueBB);
}

void CodeGen::visit(const IfStatementAST &if_statement)
{
   Value *CondV;
   if_statement.Condition->accept(*this);
   CondV = lastValue;

   /// Check the type
   if (!MagmaType::isLogicalTy(CondV))
      Error::exit(if_statement.Condition, "invalid 'if' statement: logical expected");

   Function *TheFunction = Builder->GetInsertBlock()->getParent();

   BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then",
      TheFunction);
   BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
   BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");

   Builder->CreateCondBr(CondV, ThenBB, ElseBB);
   Builder->SetInsertPoint(ThenBB);

   Value *ThenV;
   if_statement.Then->accept(*this);
   ThenV = lastValue;

   /* if the then statement as ended with a return, we need to not carry on */
   bool MergeBBRequired = false;
   if (!Builder->GetInsertBlock()->getTerminator()) {
      Builder->CreateBr(MergeBB);
      MergeBBRequired = true;
   }

   TheFunction->getBasicBlockList().push_back(ElseBB);
   Builder->SetInsertPoint(ElseBB);
   if (if_statement.Else != NULL) {
      if_statement.Else->accept(*this);
      if (!Builder->GetInsertBlock()->getTerminator()) {
         Builder->CreateBr(MergeBB);
         MergeBBRequired = true;
      }
   } else {
      Builder->CreateBr(MergeBB);
      MergeBBRequired = true;
   }

   // Emit merge block.
   if (MergeBBRequired) {
      TheFunction->getBasicBlockList().push_back(MergeBB);
      Builder->SetInsertPoint(MergeBB);
   } else {
      delete MergeBB;
   }
}

void CodeGen::visit(const WhileStatementAST &while_statement)
{
   Function *TheFunction = Builder->GetInsertBlock()->getParent();

   BasicBlock *BeforeLoopBB = BasicBlock::Create(getGlobalContext(),
      "whilestart", TheFunction);
   BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "whileloop");
   BasicBlock *AfterLoopBB = BasicBlock::Create(getGlobalContext(), "afterloop");

   /// Setup loop scope for potential break
   context.createLoopScope(BeforeLoopBB, AfterLoopBB);

   Builder->CreateBr(BeforeLoopBB);
   Builder->SetInsertPoint(BeforeLoopBB);

   Value *CondV;
   while_statement.Condition->accept(*this);
   CondV = lastValue;

   /// Do some type checking
   if (!MagmaType::isLogicalTy(CondV))
      Error::exit("Error in 'while' statement: Logical expected");

   /* create check of condition */
   Builder->CreateCondBr(CondV, LoopBB, AfterLoopBB);
   TheFunction->getBasicBlockList().push_back(LoopBB);
   Builder->SetInsertPoint(LoopBB);

   /* emit loop code */
   while_statement.Do->accept(*this);

   /* jump back to before the while loop (check there's no terminator too) */
   if (!Builder->GetInsertBlock()->getTerminator())
      Builder->CreateBr(BeforeLoopBB);

   TheFunction->getBasicBlockList().push_back(AfterLoopBB);
   Builder->SetInsertPoint(AfterLoopBB);

   context.popScope();
}

void CodeGen::visit(const RepeatStatementAST &repeat_statement)
{
   Function *TheFunction = Builder->GetInsertBlock()->getParent();

   BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "repeatloop",
      TheFunction);
   BasicBlock *AfterLoopBB = BasicBlock::Create(getGlobalContext(), "afterrepeatloop");

   /// Setup loop scope for potential break
   context.createLoopScope(LoopBB, AfterLoopBB);

   Builder->CreateBr(LoopBB);
   Builder->SetInsertPoint(LoopBB);

   /* emit loop code */
   repeat_statement.Repeat->accept(*this);

   if (!Builder->GetInsertBlock()->getTerminator()) {
      Value *CondV;
      repeat_statement.Until->accept(*this);
      CondV = lastValue;

      /// Do some type checking
      if (!MagmaType::isLogicalTy(CondV))
         Error::exit("Error in 'repeat' statement: Logical expected");

      /* create check of condition */
      Builder->CreateCondBr(CondV, AfterLoopBB, LoopBB);
   }

   TheFunction->getBasicBlockList().push_back(AfterLoopBB);
   Builder->SetInsertPoint(AfterLoopBB);

   context.popScope();
}

void CodeGen::visit(const IterativeStatementsAST &iterative_statements)
{
   iterative_statements.firstIterativeStatement()->accept(*this);
}

void CodeGen::visit(const IterativeStatementAST &iterative_statement)
{
   Function *TheFunction = Builder->GetInsertBlock()->getParent();

   /// Create the "after" block here for possible break
   BasicBlock *AfterBB = BasicBlock::Create(getGlobalContext(), "afterloop", TheFunction);

   context.createIterativeLoopScope(iterative_statement.Identifier->Name,
      Builder->GetInsertBlock(), AfterBB);

   /// Handle this case seperately, for now
   if (iterative_statement.IterableIdentifier) {
      iterative_statement.IterableIdentifier->accept(*this);
      Value* IterableIdentifierValue = lastValue;

      bool typeError = true;
      if (MagmaType::isSequenceTy(lastValue))
         typeError = false;

      if (typeError)
         Error::exit(iterative_statement.IterableIdentifier, "cannot iterate over objects of type '" + MagmaType::getTypeName(lastValue->getType()) + "'");

      /// Load the sequence size
      Value* SeqSize = MagmaType::getSequenceSize(module, Builder, IterableIdentifierValue);

      /// Setup the variable that will represent an element of the sequence
      Value *alloca = createLocal(iterative_statement.Identifier->Name,
         MagmaType::getSequenceElementType(IterableIdentifierValue));

      /// Setup the index value that will be used to pull elements from the sequence
      std::string indexName = "index";
      Value *index = createAllocaInBasicBlock(context.currentScope()->EntryBB, MagmaType::getIntegerTy(), indexName);
      Builder->CreateStore(ConstantInt::get(MagmaType::getIntegerTy(), 1, false), index);

      /// Start the before-loop code (check that we should run this loop)
      BasicBlock *BeforeLoopBB = BasicBlock::Create(getGlobalContext(), "beforeloop", TheFunction);
      Builder->CreateBr(BeforeLoopBB);
      Builder->SetInsertPoint(BeforeLoopBB);

      Value *curIndex = Builder->CreateLoad(index);
      Value* EndCond = Builder->CreateICmpUGE(SeqSize, curIndex, "loopcond");

      BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
      Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

      /// Start the loop code
      Builder->SetInsertPoint(LoopBB);

      /// Get the sequence element that corresponds to the index
      Value *Element = MagmaType::getSequenceElement(module, Builder, IterableIdentifierValue, curIndex);
      Builder->CreateStore(Element, alloca);

      /// Generate sub-iterative statement if set, otherwise construct body
      if (iterative_statement.NextStatement)
         iterative_statement.NextStatement->accept(*this);
      else if (iterative_statement.Body)
         iterative_statement.Body->accept(*this);
      else
         Error::exit("Iterative statement without next statement or body");

      if (!Builder->GetInsertBlock()->getTerminator()) {
         Value *nextValue = Builder->CreateAdd(curIndex, ConstantInt::get(MagmaType::getIntegerTy(), 1, false), "nextvar");
         Builder->CreateStore(nextValue, index);
         Builder->CreateBr(BeforeLoopBB);
      }

      Builder->SetInsertPoint(AfterBB);
      context.popScope();

      return;
   }

   Value *alloca = createLocal(iterative_statement.Identifier->Name,
      MagmaType::getIntegerTy());

   /// Compute and store the start value
   iterative_statement.Begin->accept(*this);
   Builder->CreateStore(lastValue, alloca);

   /// Start the before-loop code (check that we should run this loop)
   BasicBlock *BeforeLoopBB = BasicBlock::Create(getGlobalContext(), "beforeloop", TheFunction);
   Builder->CreateBr(BeforeLoopBB);
   Builder->SetInsertPoint(BeforeLoopBB);

   Value *curValue = Builder->CreateLoad(alloca, iterative_statement.Identifier->Name);

   iterative_statement.End->accept(*this);
   Value* EndCond = Builder->CreateICmpUGE(lastValue, curValue, "loopcond");

   BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
   Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

   /// Start the loop code
   Builder->SetInsertPoint(LoopBB);

   /// Generate sub-iterative statement if set, otherwise construct body
   if (iterative_statement.NextStatement)
      iterative_statement.NextStatement->accept(*this);
   else if (iterative_statement.Body)
      iterative_statement.Body->accept(*this);
   else
      Error::exit("Iterative statement without next statement or body");

   if (!Builder->GetInsertBlock()->getTerminator()) {
      iterative_statement.Step->accept(*this);
      Value *nextValue = Builder->CreateAdd(curValue, lastValue, "nextvar");
      Builder->CreateStore(nextValue, alloca);
      Builder->CreateBr(BeforeLoopBB);
   }

   Builder->SetInsertPoint(AfterBB);
   context.popScope();
}

void CodeGen::visit(const EvaluatedSequenceAST &evaluated_sequence)
{
   IterativeStatementsAST *IterStmts = evaluated_sequence.IterativeStatements;
   IterativeStatementAST *IterStmt;

   /// Attempt to compute the length of required sequence (speed!)
   IterStmt = IterStmts->firstIterativeStatement();
   IterStmt->Begin->accept(*this);

   if (!isa<ConstantInt>(lastValue))
      Error::exit("Evaluated sequences must have constant iterative statements for the moment");

   Constant *Begin, *End;
   Begin = cast<ConstantInt>(lastValue);

   IterStmt->End->accept(*this);

   if (!isa<ConstantInt>(lastValue))
      Error::exit("Evaluated sequences must have constant iterative statements for the moment");

   End = cast<ConstantInt>(lastValue);

   Value *Difference = Builder->CreateSub(End, Begin, "sub_seqlen");

   Builder->CreateAlloca(Type::getInt64Ty(getGlobalContext()), Difference);

   return;
}

void CodeGen::visit(const BreakStatementAST &break_statement)
{
   if (break_statement.Identifier != NULL) {
      IterativeLoopScope *loopScope = context.getIterativeLoopScope(break_statement.Identifier->Name);
      if (!loopScope)
         Error::exit(break_statement, "invalid break statement: '" +
            break_statement.Identifier->Name + "' is not a valid loop variable");
      Builder->CreateBr(loopScope->BreakBB);
   } else {
      LoopScope *loopScope = context.getLoopScope();
      if (!loopScope)
         Error::exit(break_statement, "cannot use break outside a loop environment");
      Builder->CreateBr(loopScope->BreakBB);
   }
}

void CodeGen::visit(const FunctionDeclarationStatementAST &func_decl_statement)
{
   /// Store the current insert point (restored at the end)
   BasicBlock *CurrentInsertPointBB = Builder->GetInsertBlock();

   /// Check if we need to run a look ahead
   if (context.lookAheadQueue.empty()) {
      ContextLookAhead CLA(context);
      func_decl_statement.accept(CLA);
   }

   /// Create the scope for the function and push onto stack
   /// Function signature becomes: args, imports
   FunctionScope *functionScope
      = context.createFunctionScope(func_decl_statement.Name->Name, NULL);
   size_t total_imports = functionScope->Imports.size();

   /// Create the function arguments (the actual arguments)
   std::vector<std::string> TheArgumentNames(func_decl_statement.Arguments->size() + total_imports);
   std::vector<Type*> TheArgumentTypes(TheArgumentNames.size());
   std::vector<bool> TheArgumentIsReference(TheArgumentNames.size());

   int i = 0;
   IdentifierList::const_iterator IL_it = func_decl_statement.Arguments->begin();
   IdentifierList::const_iterator IL_end = func_decl_statement.Arguments->end();
   for (; IL_it != IL_end; ++IL_it, ++i) {
      TheArgumentNames[i] = (*IL_it)->Name;
      (*IL_it)->Type->accept(*this);
      if ((*IL_it)->IsReference) {
         TheArgumentIsReference[i] = true;
         if (!func_decl_statement.IsProcedure)
            Error::exit((*IL_it), "invalid parameter '" + (*IL_it)->Name +
               "': reference parameters are only allowed in procedures");
         lastType = lastType->getPointerTo(0);
      }
      TheArgumentTypes[i] = lastType;
   }

   /// Create the imported arguments information
   StringSet::const_iterator imports_it = functionScope->Imports.begin();
   for (; imports_it != functionScope->Imports.end(); ++imports_it, ++i)
      TheArgumentNames[i] = *imports_it;

   PointerType *PT = NULL;
   for (i = func_decl_statement.Arguments->size(); i < TheArgumentNames.size(); ++i)
      if ((PT = dyn_cast<PointerType>(context.getLocal(TheArgumentNames[i])->getType())))
         TheArgumentTypes[i] = PT->getElementType();
      else Error::exit("Alloca does not have pointer type!");

   /// Determine the function return type
   if (func_decl_statement.IsProcedure)
      lastType = Type::getVoidTy(getGlobalContext());
   else
      func_decl_statement.Name->Type->accept(*this);

   /// Construct the function type from the argument types
   FunctionType *TheFunctionType = FunctionType::get(lastType, makeArrayRef(TheArgumentTypes), false);

   /// Construct the function
   Function *TheFunction = Function::Create(TheFunctionType, GlobalValue::InternalLinkage,
      func_decl_statement.Name->Name, module);

   /// Create the entry block for the function
   BasicBlock *EntryBB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction, 0);
   functionScope->EntryBB = EntryBB;

   Builder->SetInsertPoint(EntryBB);

   /// Setup arguments for use in function
   Function::arg_iterator AI = TheFunction->arg_begin();
   for (i = 0; i < TheArgumentNames.size(); ++AI, ++i) {
      AI->setName(TheArgumentNames[i]);
      if (TheArgumentIsReference[i]) {
         /// Create alloc for pointer (not registered)
         Value *Alloca = createAllocaInBasicBlock(context.currentScope()->EntryBB, AI->getType(), TheArgumentNames[i]+"ptr");
         /// Store the param in it
         Builder->CreateStore(AI, Alloca);
         Alloca = Builder->CreateLoad(Alloca, TheArgumentNames[i]);
         context.currentScope()->addLocal(TheArgumentNames[i], Alloca);
      } else {
         Builder->CreateStore(AI, createLocal(TheArgumentNames[i],
            TheArgumentTypes[i]));
      }
   }

   /// Create a variable to represent the current function for recursive calls
   if (functionScope->isRecursive) {
      Type* magmaFunctionType = MagmaType::getFunctionType(TheFunction,
         total_imports);
      Value *FunctionDefinition = MagmaType::createFunction(module, Builder, TheFunction, total_imports);
      Value *functionDefinitionGEPIdx[2] = {
         ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
         (ConstantInt*)NULL
      };
      Value *functionParamPtr, *functionParamValue, *Alloca;
      i = TheArgumentNames.size() - total_imports;
      for (int GEPIndex = 1; i < TheArgumentNames.size(); ++i, ++GEPIndex) {
         Alloca = context.getLocal(TheArgumentNames[i]);
         if (!Alloca)
            Error::exit("Could not find local for import of '"+ TheArgumentNames[i]
               + "' into function '" + func_decl_statement.Name->Name + "'");
         functionParamValue = Builder->CreateLoad(Alloca);
         functionDefinitionGEPIdx[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), GEPIndex, true);
         functionParamPtr = Builder->CreateInBoundsGEP(FunctionDefinition, functionDefinitionGEPIdx, TheArgumentNames[i] + "_ptr");
         Builder->CreateStore(functionParamValue, functionParamPtr);
      }

      Alloca = createLocal(
         func_decl_statement.Name->Name, FunctionDefinition->getType());
      Builder->CreateStore(FunctionDefinition, Alloca);
   }

   /// Emit the function body code
   func_decl_statement.Body->accept(*this);

   if (!Builder->GetInsertBlock()->getTerminator()) {
      if (!func_decl_statement.IsProcedure)
         Error::exit(func_decl_statement, "return statement required in function '" +
             func_decl_statement.Name->Name + "'");
      Builder->CreateRetVoid();
   }

   /// Check the function
   if (!SkipVerify)
      verifyFunction(*TheFunction);

   /// Pop the function scope
   context.popScope();

   /// Restore original insert point
   Builder->SetInsertPoint(CurrentInsertPointBB);

   /// Create an alloca in the current scope for the function, and load the
   /// required values to construct the environment
   Value *FunctionDefinition = MagmaType::createFunction(module, Builder, TheFunction, total_imports);

   /// Find the function parameters that are imports
   i = TheArgumentNames.size() - total_imports;

   /// The imports have already been setup in the correct scopes, so we can just
   /// refer to them directly here
   Value *functionDefnitionGEPIndx[2] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false), NULL
   };
   Value *functionParamPtr;
   Value *functionParamValue;
   Value *Alloca;
   for (int GEPIndex = 1; i < TheArgumentNames.size(); ++i, ++GEPIndex) {
      Alloca = context.getLocal(TheArgumentNames[i]);
      if (!Alloca)
         Error::exit("Could not find local for import of '" + TheArgumentNames[i]
            + "' into function '" + func_decl_statement.Name->Name + "'");
      functionParamValue = Builder->CreateLoad(Alloca);
      functionDefnitionGEPIndx[1] = ConstantInt::get(Type::getInt32Ty(getGlobalContext()), GEPIndex, true);
      functionParamPtr = Builder->CreateInBoundsGEP(FunctionDefinition, functionDefnitionGEPIndx, TheArgumentNames[i] + "_ptr");
      Builder->CreateStore(functionParamValue, functionParamPtr);
   }

   Alloca = getOrCreateLocalForAssignment(
      func_decl_statement.Name->Name, FunctionDefinition->getType(), func_decl_statement.Name);
   Builder->CreateStore(FunctionDefinition, Alloca);
}

void CodeGen::visit(const ReturnStatementAST &return_statement)
{
   Function *TheFunction = Builder->GetInsertBlock()->getParent();
   Type* ReturnType = TheFunction->getReturnType();

   if (return_statement.Expression) {
      return_statement.Expression->accept(*this);

      /// Verify that the return type is correct
      if (!MagmaType::typesAreEqual(ReturnType, lastValue->getType())) {
         if (!ReturnType->isVoidTy())
            Error::exit(return_statement, "return type of " +
               MagmaType::getTypeName(lastValue->getType()) +
               " is not valid for function of type " +
               MagmaType::getTypeName(ReturnType));
         else
            Error::exit(return_statement, "values cannot be returned from procedures");
      }
      Builder->CreateRet(lastValue);
      return;
   }

   if (!ReturnType->isVoidTy())
      Error::exit(return_statement, "empty return value invalid for function of type " +
          MagmaType::getTypeName(TheFunction->getReturnType()));
   Builder->CreateRetVoid();
}

void CodeGen::visit(const CallExpressionAST &call_expression)
{
   Value *CalleeFAlloca = context.getLocal(call_expression.Name->Name);
   if (!CalleeFAlloca) {
      Error::exit(call_expression.Name, "invalid call: '" + call_expression.Name->Name +
         "' is not a valid function/procedure name");
   }

   Value *CalleeFPtr = Builder->CreateLoad(CalleeFAlloca);

   if (PointerType *CalleeFPtrType = dyn_cast<PointerType>(CalleeFPtr->getType())) {
      if (MagmaType::isFunctionTy(CalleeFPtrType->getElementType())) {
         // Find the function pointer
         Value* functionPtr = MagmaType::getFunctionPtr(module, Builder, CalleeFPtr);
         PointerType *functionPtrType = cast<PointerType>(functionPtr->getType());
         FunctionType *functionPtrFunctionType = cast<FunctionType>(functionPtrType->getElementType());

         int i = 0;
         std::vector<Value *> ArgsV;
         ExpressionList::const_iterator it;
         FunctionType::param_iterator PI = functionPtrFunctionType->param_begin();
         for (it = call_expression.Arguments->begin(); it != call_expression.Arguments->end(); ++it, ++PI, ++i) {
            (*it)->accept(*this);
            if (lastValue->getType() != *PI) {
               std::stringstream str;
               str << "Type of argument (" << i << ") is incorrect";
               Error::exit(str.str());
            }
            ArgsV.push_back(lastValue);
         }

         // Go through the imports and indirect imports
         i = 0;
         for (; PI != functionPtrFunctionType->param_end(); PI++, i++) {
            // this should NOT fail, as it's an import...
            ArgsV.push_back(MagmaType::getFunctionParam(module, Builder, CalleeFPtr, i));
         }
         // Actually perform the function call
         lastValue = Builder->CreateCall(functionPtr, ArgsV);
         return;
      }
   }

   Error::exit(call_expression.Name, "invalid call: '" + call_expression.Name->Name +
      "' is not a valid function/procedure name");
}
