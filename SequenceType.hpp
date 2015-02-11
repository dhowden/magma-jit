// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef SEQUENCETYPE_HPP_EMU7XFFL
#define SEQUENCETYPE_HPP_EMU7XFFL

class SequenceType {
   /// Sequence StructType creation
   static StructType *createLLVMStructType(Type *elementType)
   {
      std::vector<Type *> ElementTypes(2);
      ElementTypes[0] = Type::getInt32Ty(getGlobalContext());
      ElementTypes[1] = elementType;

      return StructType::Create(getGlobalContext(), makeArrayRef(ElementTypes), "sequence", false);
   }
};

#endif /* end of include guard: SEQUENCETYPE_HPP_EMU7XFFL */
