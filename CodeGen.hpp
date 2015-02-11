// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef CODEGEN_HPP_PCUMK2KK
#define CODEGEN_HPP_PCUMK2KK

#include <set>
#include <stack>
#include <queue>
#include <vector>
#include <sstream>
#include <typeinfo>
#include <iostream>

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/PassManager.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Scalar.h>

#include <llvm/Support/CommandLine.h>

#include "Error.hpp"
#include "Visitor.hpp"
#include "MagmaType.hpp"

#include "Scope.hpp"
#include "Context.hpp"

using namespace llvm;

extern cl::opt<bool> EmitScopeDump;

//===----------------------------------------------------------------------===//
// CodeGen class: Generates LLVM IR from AST using visitor pattern
//===----------------------------------------------------------------------===//

class CodeGen : public Visitor {
public:
   Module *module;
   Function *mainFunction;

   IRBuilder<> *Builder;
   Value *lastValue;
   Type *lastType;

   Context context;

   CodeGen();
   virtual ~CodeGen();

   //===-------------------------------------------------------------------===//
   // Root CodeGen methods
   //===-------------------------------------------------------------------===//

   void generateCode(BlockAST &block);
   GenericValue runCode();

   //===-------------------------------------------------------------------===//
   // Variable code generation methods
   //===-------------------------------------------------------------------===//

   Value* createLocal(const std::string &name, Type* type);
   Value* createLocalInScope(const std::string &name, Type* type, Scope* scope);
   Value* createAllocaInBasicBlock(BasicBlock* EntryBB, Type* type, const std::string &name);
   Value* getOrCreateLocalForAssignment(const std::string &name, Type *type, NodeAST &node);
   Value* getOrCreateLocalForAssignment(const std::string &name, Type *type, NodeAST *node);

   //===-------------------------------------------------------------------===//
   // Visitor pattern methods
   //===-------------------------------------------------------------------===//
#ifdef NODE
#undef NODE
#endif
#define NODE(x) virtual void visit(const x &);
#include "ASTNodeTypes.def"

   //===-------------------------------------------------------------------===//
   // Internal (Extern C) function integration helpers
   //===-------------------------------------------------------------------===//

   Value* createStringPtr(Value* String);
   void createPrintfCall(Value* Format, std::vector<Value *> &Args);

   void printBoolElt(Value *Val);
   void printValue(Value *Val);
   Value* createConstantStringGlobal(std::string value, std::string name = "str");

};

#endif /* end of include guard: CODEGEN_HPP_PCUMK2KK */
