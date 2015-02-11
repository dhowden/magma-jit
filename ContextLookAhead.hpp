// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef CONTEXTLOOKAHEAD_HPP_J7IG5LB8
#define CONTEXTLOOKAHEAD_HPP_J7IG5LB8

#include "CodeGen.hpp"

using namespace llvm;

//===----------------------------------------------------------------------===//
// ContextLookAhead class: does a dry-run of AST to determine how to setup
// function parameters etc...
//===----------------------------------------------------------------------===//

class ContextLookAhead : public Visitor {
public:
   ContextLookAhead(Context &ctx) : context(ctx) {}
   virtual ~ContextLookAhead() {}

   Context& context;

   //===-------------------------------------------------------------------===//
   // Variable handling methods to mimic those of CodeGen
   //===-------------------------------------------------------------------===//

   void createLocal(const std::string& name);
   void createLocalInScope(const std::string& name, Scope* scope);
   void getOrCreateLocalForAssignment(const std::string& name);

   //===-------------------------------------------------------------------===//
   // Visitor pattern methods
   //===-------------------------------------------------------------------===//
#ifdef NODE
#undef NODE
#endif
#define NODE(x) virtual void visit(const x &);
#include "ASTNodeTypes.def"
};


#endif /* end of include guard: CONTEXTLOOKAHEAD_HPP_J7IG5LB8 */
