// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef MAGMATYPE_HPP_LC4X5P1E
#define MAGMATYPE_HPP_LC4X5P1E

#include <string>
#include <sstream>

#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/CallingConv.h>

#include <llvm/Support/raw_ostream.h>

#include "Error.hpp"

using namespace llvm;

/// This class acts as a bridge between Magma types and their LLVM equivalents
class MagmaType {
public:
   // Some helper function (should be moved at later date!)
   static Function* reallocFunction(Module *module);
   static Function* mallocFunction(Module *module);

   /// Logical type (LLVM i1)
   static bool isLogicalTy(Type *type);
   static bool isLogicalTy(Value *value) { return isLogicalTy(value->getType()); }
   static IntegerType* getLogicalTy();

   /// Integer type (LLVM i64)
   static bool isIntegerTy(Type *type);
   static bool isIntegerTy(Value *value) { return isIntegerTy(value->getType()); }
   static IntegerType* getIntegerTy();

   /// Function type (LLVM { *(functionPtr), import args, indirect import args })
   static bool isFunctionTy(Type *type);
   static bool isFunctionTy(Value *value) { return isFunctionTy(value->getType()); }
   static Value* getFunctionPtr(Module* module, IRBuilder<> *Builder, Value *function);
   static Value* getFunctionPtrPtr(Module* module, IRBuilder<> *Builder, Value *function);
   static Value* getFunctionParam(Module* module, IRBuilder<> *Builder, Value *function, size_t index);
   static Value* getFunctionParamPtr(Module* module, IRBuilder<> *Builder, Value *function, size_t index);

   static Type* getFunctionType(Function* function, size_t total_imports);
   static Value* getFunctionStructSizeForMalloc(Module* module, IRBuilder<> *Builder, Type *functionType, size_t total_imports);
   static Value* createFunction(Module* module, IRBuilder<> *Builder, Function *function, size_t total_imports);

   /// Tuple type
   static PointerType* getTupleTy(const std::vector<Type*> &elementTypes);
   static Value* createTuple(Module* module, IRBuilder<> *Builder, const std::vector<Type *> &elementTypes);
   static Value* getTupleStructSizeForMalloc(Module* module, IRBuilder<> *Builder, Type* tupleType);
   static Value* getTupleElementPtr(Module* module, IRBuilder<> *Builder, Value *tuple, Value *index);
   static Value* getTupleElement(Module* module, IRBuilder<> *Builder, Value *tuple, Value *index)
      { return Builder->CreateLoad(getTupleElementPtr(module, Builder, tuple, index)); }
   static Value* getTupleSizePtr(Module* module, IRBuilder<> *Builder, Value *tuple);
   static Value* getTupleSize(Module* module, IRBuilder<> *Builder, Value *tuple)
      { return Builder->CreateLoad(getTupleSizePtr(module, Builder, tuple)); }
   static bool isTupleTy(Type *type);
   static bool isTupleTy(Value *value) { return isTupleTy(value->getType()); }
   static std::vector<Type *> getTupleElementTypes(Type *type);
   static std::vector<Type *> getTupleElementTypes(PointerType *pointerType);

   /// Sequence type
   static bool isSequenceTy(Type *type);
   static bool isSequenceTy(Value *value) { return isSequenceTy(value->getType()); }
   static Type* getSequenceElementType(Type *type);
   static Type* getSequenceElementType(PointerType *pointerType);
   static Type* getSequenceElementType(Value *value) {
      if (PointerType *PT = dyn_cast<PointerType>(value->getType()))
         return getSequenceElementType(PT);
      Error::exit("could not determine sequence element type");
      return NULL; /// for compiler warning!
   }
   static bool isSequenceWithElementType(Value *sequence, Type *type) {
      return isSequenceTy(sequence) && getSequenceElementType(sequence) == type;
   }
   static PointerType* getSequenceTy(Type *elementType);

   /// Sequence handling methods
   static Value* getSequenceSize(Module* module, IRBuilder<> *Builder, Value *sequence)
      { return Builder->CreateLoad(getSequenceSizePtr(module, Builder, sequence)); }
   static Value* getSequenceElement(Module* module, IRBuilder<> *Builder, Value *sequence, Value *index)
      { return Builder->CreateLoad(getSequenceElementPtr(module, Builder, sequence, index)); }
   static Value* getSequenceStructSizeForMalloc(Module* module, IRBuilder<> *Builder, Type *sequenceType, Value *requiredSize);
   static Value* getSequenceSizePtr(Module* module, IRBuilder<> *Builder, Value *sequence);
   static Value* getSequenceElementPtr(Module* module, IRBuilder<> *Builder, Value *sequence, Value *index);
   static Value* createSequence(Module* module, IRBuilder<> *Builder, Type *elementType, Value *size);
   static void resizeSequence(Module* module, IRBuilder<> *Builder, Value *sequencePtr, Value *sequence, Value *requiredSize);

   /// String type (LLVM [n x i8])
   static bool isStringTy(Type *type);
   static bool isStringTy(Value *value) { return isStringTy(value->getType()); }
   ///TODO: Implement strings as [0 x i8]
   ///static ArrayType* getStringTy();

   /// Type -> std::string conversion methods
   static std::string getTypeName(Type *type);
   static std::string getTypeName(Value *value) { return getTypeName(value->getType()); }

   /// std::string -> Type conversion methods
   static Type* constructMagmaType(const std::string &str, const std::vector<Type*> &elementTypes);

   /// Type structure equality checking...
   static bool typesAreEqual(Type *typeA, Type *typeB);
};

#endif /* end of include guard: MAGMATYPE_HPP_LC4X5P1E */
