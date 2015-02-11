// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef VISITOR_HPP_X8L7NJF8
#define VISITOR_HPP_X8L7NJF8

#include <iostream>

#include "AST.hpp"

class Visitor {
public:
   Visitor() {}
   virtual ~Visitor() {}
#ifdef NODE
#undef NODE
#endif
#define NODE(x) virtual void visit(const x &) = 0;
#include "ASTNodeTypes.def"
};

#endif /* end of include guard: VISITOR_HPP_X8L7NJF8 */