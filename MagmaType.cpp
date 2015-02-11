// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "MagmaType.hpp"

bool MagmaType::isLogicalTy(Type *type)
{
    return type->isIntegerTy(1);
}

IntegerType* MagmaType::getLogicalTy()
{
    return Type::getInt1Ty(getGlobalContext());
}

bool MagmaType::isIntegerTy(Type *type)
{
    return type->isIntegerTy(64);
}

IntegerType* MagmaType::getIntegerTy()
{
    return Type::getInt64Ty(getGlobalContext());
}

/// Function type identification
/// { (*funct ptr), import args, indirect import args }
bool MagmaType::isFunctionTy(Type *type)
{
   if (!type->isStructTy())
      return false;
   StructType *TheStructType = cast<StructType>(type);

   if (!TheStructType->getNumElements())
      return false;

   if (PointerType *pointerType = dyn_cast<PointerType>(TheStructType->getElementType(0)))
      if (pointerType->getElementType()->isFunctionTy())
         return true;

   return false;
}

Type* MagmaType::getFunctionType(Function* function, size_t total_imports)
{
   FunctionType *functionType = function->getFunctionType();
   std::vector<Type *> ElementTypes(1 + total_imports);
   ElementTypes[0] = functionType->getPointerTo(0);
   for (uint i = 0; i < total_imports; ++i)
      ElementTypes[1+i] = functionType->getParamType(functionType->getNumParams() - total_imports + i);
   return StructType::create(getGlobalContext(), makeArrayRef(ElementTypes), "function_"+std::string(function->getName()), false);
}

Value* MagmaType::getFunctionPtrPtr(Module* module, IRBuilder<> *Builder, Value *function)
{
   Value *functionPtrGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, true)
   };
   return Builder->CreateInBoundsGEP(function, makeArrayRef(functionPtrGEPIdx), function->getName() + "_functionptrptr");
}

Value* MagmaType::getFunctionPtr(Module* module, IRBuilder<> *Builder, Value *function)
{
   Value *ptr = getFunctionPtrPtr(module, Builder, function);
   return Builder->CreateLoad(ptr);
}

Value *MagmaType::getFunctionParamPtr(Module* module, IRBuilder<> *Builder, Value *function, size_t index)
{
   Value *functionParamPtrGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), index+1, true)
   };
   return Builder->CreateInBoundsGEP(function, makeArrayRef(functionParamPtrGEPIdx));
}

Value *MagmaType::getFunctionParam(Module* module, IRBuilder<> *Builder, Value *function, size_t index)
{
   return Builder->CreateLoad(getFunctionParamPtr(module, Builder, function, index));
}

Value* MagmaType::getFunctionStructSizeForMalloc(Module* module, IRBuilder<> *Builder, Type *functionType, size_t total_imports)
{
   Value* functionStructSizeGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1, false),
      // ConstantInt::get(Type::getInt32Ty(getGlobalContext()), total_imports, true),
   };
   Value *functionStructSize = Builder->CreateGEP(
      ConstantInt::getNullValue(functionType),
      makeArrayRef(functionStructSizeGEPIdx)
   );
   return Builder->CreatePtrToInt(functionStructSize, Type::getInt64Ty(getGlobalContext()));
}

Value* MagmaType::createFunction(Module* module, IRBuilder<> *Builder, Function *function, size_t total_imports)
{
   Type* TheFunctionType = MagmaType::getFunctionType(function, total_imports);
   Value *Result = Builder->CreateAlloca(TheFunctionType, 0, function->getName()+"_ptr");
   Value *functionPtr = getFunctionPtrPtr(module, Builder, Result);
   Builder->CreateStore(function, functionPtr);
   // PointerType *TheFunctionTypePtr = MagmaType::getFunctionType(function, total_imports)->getPointerTo(0);
   // Value *SizeU = getFunctionStructSizeForMalloc(module, Builder, TheFunctionTypePtr, total_imports);
   // 
   // Function *mallocFunction = module->getFunction("malloc");
   // if (!mallocFunction) {
   //    Type* mallocFunctionArgs[] = { Type::getInt64Ty(getGlobalContext()) };
   //    FunctionType *mallocFunctionType = FunctionType::get(Type::getInt8Ty(getGlobalContext())->getPointerTo(0),
   //       makeArrayRef(mallocFunctionArgs), false);
   //    mallocFunction = Function::Create(mallocFunctionType, GlobalValue::ExternalLinkage, "malloc", module);
   // }
   // Value *Ptr = Builder->CreateCall(mallocFunction, SizeU);
   // 
   // Value *Result = Builder->CreateBitCast(Ptr, TheFunctionTypePtr);

   return Result;
}

/// Tuple type identification
/// { i32, types... }
bool MagmaType::isTupleTy(Type *type)
{
   if (!type->isPointerTy())
      return false;

   PointerType *ThePointerType = cast<PointerType>(type);

   if (!ThePointerType->getElementType()->isStructTy())
      return false;

   StructType *TheStructType = cast<StructType>(ThePointerType->getElementType());

   if (TheStructType->getNumElements() < 2)
      return false;
   if (!MagmaType::isIntegerTy(TheStructType->getElementType(0)))
      return false;

   if (!TheStructType->getElementType(1)->isArrayTy()) {
      return true;
   }
   return false;
}

Value* MagmaType::getTupleSizePtr(Module* module, IRBuilder<> *Builder, Value *tuple)
{
   Value *tupleSizeGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, true)
   };
   return Builder->CreateInBoundsGEP(tuple, makeArrayRef(tupleSizeGEPIdx));
}

Value* MagmaType::getTupleElementPtr(Module* module, IRBuilder<> *Builder, Value *tuple, Value *index)
{
   if (ConstantInt *CI = dyn_cast<ConstantInt>(index)) {
      Value* tupleElementGEPIdx[] = {
         ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
         // ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1, true)
         Builder->CreateTrunc(index, Type::getInt32Ty(getGlobalContext()))
      };
      return Builder->CreateInBoundsGEP(tuple, makeArrayRef(tupleElementGEPIdx), "element_ptr");
   }
   Error::exit("Must use constant indexing for tuples");

   /// Prevent compiler warnings
   return NULL;
}

Value* MagmaType::getTupleStructSizeForMalloc(Module* module, IRBuilder<> *Builder, Type* tupleType)
{
   Value* tupleStructSizeGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1, true),
   };
   Value *requiredStructSize = Builder->CreateGEP(
      ConstantInt::getNullValue(tupleType),
      makeArrayRef(tupleStructSizeGEPIdx)
   );
   return Builder->CreatePtrToInt(requiredStructSize, Type::getInt64Ty(getGlobalContext()));
}

Value* MagmaType::createTuple(Module* module, IRBuilder<> *Builder, const std::vector<Type *> &elementTypes)
{
   PointerType *TheTupleType = MagmaType::getTupleTy(elementTypes);
   Value *SizeU = getTupleStructSizeForMalloc(module, Builder, TheTupleType);

   Function *mallocFunction = MagmaType::mallocFunction(module);
   Value *Ptr = Builder->CreateCall(mallocFunction, SizeU);

   Value *Result = Builder->CreateBitCast(Ptr, TheTupleType);
   Value *SizePtr = getTupleSizePtr(module, Builder, Result);
   Builder->CreateStore(ConstantInt::get(getIntegerTy(), elementTypes.size(), false), SizePtr);

   return Result;
}

PointerType* MagmaType::getTupleTy(const std::vector<Type *> &elementTypes)
{
   std::vector<Type *> ElementTypes(elementTypes.size() + 1);
   ElementTypes[0] = getIntegerTy();
   for (unsigned int i = 1; i <= elementTypes.size(); ++i)
      ElementTypes[i] = elementTypes[i-1];

   std::stringstream str;
   str << "tuple_" << elementTypes.size();

   return StructType::create(getGlobalContext(), makeArrayRef(ElementTypes), str.str(), false)->getPointerTo(0);
}


std::vector<Type *> MagmaType::getTupleElementTypes(Type *type)
{
   PointerType *PT = dyn_cast<PointerType>(type);
   return getTupleElementTypes(PT);
}

std::vector<Type *> MagmaType::getTupleElementTypes(PointerType *pointerType)
{
   std::vector<Type *> result;
   if (StructType *ST = dyn_cast<StructType>(pointerType->getElementType())) {
      StructType::element_iterator it = ST->element_begin();
      if (it != ST->element_end()) {
         /// Skip the first element (this is the size)
         ++it;
         for (; it != ST->element_end(); it++) {
            result.push_back(*it);
         }
      }
   }
   return result;
}

/// Sequence type identification
/// *{ i32, [ 0 x elt_type ] }
bool MagmaType::isSequenceTy(Type *type)
{
   if (!type->isPointerTy())
      return false;

   PointerType *ThePointerType = cast<PointerType>(type);

   if (!ThePointerType->getElementType()->isStructTy())
      return false;

   StructType *TheStructType = cast<StructType>(ThePointerType->getElementType());

   if (TheStructType->getNumElements() != 2)
      return false;
   if (!MagmaType::isIntegerTy(TheStructType->getElementType(0)))
      return false;

   if (TheStructType->getElementType(1)->isArrayTy()) {
      ArrayType *TheArrayType = cast<ArrayType>(TheStructType->getElementType(1));
      if (TheArrayType->getNumElements() != 0) {
         return false;
      }
      return true;
   }
   return false;
}

Type* MagmaType::getSequenceElementType(Type *type)
{
   PointerType *PT = dyn_cast<PointerType>(type);
   return getSequenceElementType(PT);
}

Type* MagmaType::getSequenceElementType(PointerType *pointerType)
{
   StructType *ST = dyn_cast<StructType>(pointerType->getElementType());
   ArrayType *AT = cast<ArrayType>(ST->getTypeAtIndex(1));
   return AT->getElementType();
}

Value* MagmaType::getSequenceSizePtr(Module* module, IRBuilder<> *Builder, Value *sequence)
{
   Value *sequenceSizeGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, true)
   };
   return Builder->CreateInBoundsGEP(sequence, makeArrayRef(sequenceSizeGEPIdx));
}

Value* MagmaType::getSequenceElementPtr(Module* module, IRBuilder<> *Builder, Value *sequence, Value *index)
{
   Value* sequenceElementGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1, true),
      Builder->CreateSub(index,
         ConstantInt::get(MagmaType::getIntegerTy(), 1, true), "index")
   };
   return Builder->CreateInBoundsGEP(sequence, makeArrayRef(sequenceElementGEPIdx), "element_ptr");
}

Value* MagmaType::getSequenceStructSizeForMalloc(Module* module, IRBuilder<> *Builder, Type *sequenceType, Value *requiredSize)
{
   Value* sequenceStructSizeGEPIdx[] = {
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 0, false),
      ConstantInt::get(Type::getInt32Ty(getGlobalContext()), 1, true),
      requiredSize
   };
   Value *requiredStructSize = Builder->CreateGEP(
      ConstantInt::getNullValue(sequenceType),
      makeArrayRef(sequenceStructSizeGEPIdx)
   );
   return Builder->CreatePtrToInt(requiredStructSize, Type::getInt64Ty(getGlobalContext()));
}

Value* MagmaType::createSequence(Module* module, IRBuilder<> *Builder, Type *elementType, Value *size)
{
   PointerType *TheSequenceType = MagmaType::getSequenceTy(elementType);
   Value *SizeU = getSequenceStructSizeForMalloc(module, Builder, TheSequenceType, size);

   Function *mallocFunction = MagmaType::mallocFunction(module);
   Value *Ptr = Builder->CreateCall(mallocFunction, SizeU);

   Value *Result = Builder->CreateBitCast(Ptr, TheSequenceType);
   Value *SizePtr = getSequenceSizePtr(module, Builder, Result);
   Builder->CreateStore(size, SizePtr);

   return Result;
}

Function* MagmaType::mallocFunction(Module *module)
{
   Function *Malloc = module->getFunction("malloc");
   if (!Malloc) {
      Type* mallocFunctionArgs[] = { Type::getInt64Ty(getGlobalContext()) };
      FunctionType *mallocFunctionType = FunctionType::get(Type::getInt8Ty(getGlobalContext())->getPointerTo(0),
         makeArrayRef(mallocFunctionArgs), false);
      Malloc = Function::Create(mallocFunctionType, GlobalValue::ExternalLinkage, "malloc", module);
   }

   return Malloc;
}

Function* MagmaType::reallocFunction(Module *module)
{
   Function *Realloc = module->getFunction("realloc");
   if (!Realloc) {
      Type* reallocFunctionArgs[] = {
         Type::getInt8Ty(getGlobalContext())->getPointerTo(0),
         Type::getInt64Ty(getGlobalContext())
      };
      FunctionType *reallocFunctionType = FunctionType::get(Type::getInt8Ty(getGlobalContext())->getPointerTo(0),
         makeArrayRef(reallocFunctionArgs), false);
      Realloc = Function::Create(reallocFunctionType, GlobalValue::ExternalLinkage, "realloc", module);
   }

   return Realloc;
}

/// Must have sequence from load, i.e. sequence = Builder->CreateLoad(...)
void MagmaType::resizeSequence(Module* module, IRBuilder<> *Builder, Value *sequencePtr, Value *sequence, Value *requiredSize)
{
   Value *SizeV = getSequenceSize(module, Builder, sequence);

   Function *TheFunction = Builder->GetInsertBlock()->getParent();
   BasicBlock *ResizeBB = BasicBlock::Create(getGlobalContext(), "resize",
      TheFunction);
   BasicBlock *ContinueBB = BasicBlock::Create(getGlobalContext(), "continue");

   Value *CondV = Builder->CreateICmpUGT(requiredSize, SizeV);
   Builder->CreateCondBr(CondV, ResizeBB, ContinueBB);

   /// Create the resizing code
   Builder->SetInsertPoint(ResizeBB);

   Function *reallocFunction = MagmaType::reallocFunction(module);
   Value *requiredReallocSize = getSequenceStructSizeForMalloc(module, Builder, sequence->getType(), requiredSize);

   /// Cast sequence* back to char* for calling realloc
   Value *LoadPtr = Builder->CreateBitCast(sequence, Type::getInt8Ty(getGlobalContext())->getPointerTo(0));
   Value *reallocArgs[] = { LoadPtr, requiredReallocSize };
   Value *Ptr = Builder->CreateCall(reallocFunction, reallocArgs);

   /// Cast the char* back to sequence*
   Value *Result = Builder->CreateBitCast(Ptr, sequence->getType());

   Value *ResultSizePtr = getSequenceSizePtr(module, Builder, Result);
   Builder->CreateStore(requiredSize, ResultSizePtr);

   /// Store the resized sequence back in the variable
   Builder->CreateStore(Result, sequencePtr);

   /// Branch to the normal case (no resizing)
   Builder->CreateBr(ContinueBB);

   Builder->SetInsertPoint(ContinueBB);
   TheFunction->getBasicBlockList().push_back(ContinueBB);

   /// If a change has been made, it's been stored back in the pointer...
}

PointerType* MagmaType::getSequenceTy(Type *elementType)
{
   std::vector<Type *> ElementTypes(2);
   ElementTypes[0] = getIntegerTy();
   ElementTypes[1] = ArrayType::get(elementType, 0);

   // std::stringstream str;
   // str << "sequence_" << getTypeName(elementType);
   // return StructType::create(getGlobalContext(), makeArrayRef(ElementTypes), str.str(), false);
   return StructType::get(getGlobalContext(), makeArrayRef(ElementTypes))->getPointerTo(0);
}

/// String type identification
/// [n x i8]
bool MagmaType::isStringTy(Type *type)
{
   if (PointerType* PT = dyn_cast<PointerType>(type))
      return PT->getElementType()->isIntegerTy(8);

   return false;
}

std::string MagmaType::getTypeName(Type *type)
{
   if (isLogicalTy(type))
      return std::string("BoolElt");
   if (isIntegerTy(type))
      return std::string("RngIntElt");
   if (isStringTy(type))
      return std::string("MonStgElt");
   if (isSequenceTy(type)) {
      std::stringstream str;
      str << "SeqEnum[" << getTypeName(getSequenceElementType(type)) << "]";
      return str.str();
   }
   if (isTupleTy(type)) {
      std::vector<Type *> elementTypes = getTupleElementTypes(type);
      std::stringstream str;
      for (std::vector<Type *>::iterator it = elementTypes.begin(); it != elementTypes.end(); ++it)
         str << ", " << getTypeName(*it);
      return "Tup[" + str.str().substr(2) + "]";
   }
   if (PointerType* PT = dyn_cast<PointerType>(type)) {
      if (isFunctionTy(PT->getElementType())) {
         return std::string("UserProgram");
      }
      return "~" + getTypeName(PT->getElementType());
   }

   std::string tempstring;
   raw_string_ostream str(tempstring);
   str << "Type(";
   type->print(str);
   str << ")";

   return str.str();
}

Type* MagmaType::constructMagmaType(const std::string &str, const std::vector<Type*> &elementTypes)
{
   std::string sequence("SeqEnum"), tuple("Tup"), integer("RngIntElt"),
      logical("BoolElt");
   if (str == logical) {
      return getLogicalTy();
   } else if (str == integer) {
      return getIntegerTy();
   } else if (str == sequence) {
      return getSequenceTy(elementTypes[0]);
   } else if (str == tuple) {
      return getTupleTy(elementTypes);
   }
   return NULL;
}

bool MagmaType::typesAreEqual(Type *typeA, Type *typeB)
{
   if (typeA == typeB) return true;
   if (typeA->isPointerTy()) {
      if (!typeB->isPointerTy()) return false;
      PointerType *PointerTypeA = cast<PointerType>(typeA);
      PointerType *PointerTypeB = cast<PointerType>(typeB);
      return typesAreEqual(PointerTypeA->getElementType(), PointerTypeB->getElementType());
   }
   if (typeA->isStructTy()) {
      if (!typeB->isStructTy()) return false;
      StructType *StructTypeA = cast<StructType>(typeA);
      StructType *StructTypeB = cast<StructType>(typeB);
      return StructTypeA->isLayoutIdentical(StructTypeB);
   }
   return false;
}
