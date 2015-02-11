// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef PRINTER_HPP_JHX1R2VX
#define PRINTER_HPP_JHX1R2VX

#include "Visitor.hpp"

#include <string>

const std::string INDENT_UNIT("   ");

class Printer : public Visitor {
   std::ostream& Stream;
   unsigned int indentLevel;
public:
   Printer(std::ostream& stream) : Stream(stream), indentLevel(0) {}
   virtual ~Printer() {}

   void nl();
   void increaseIndent();
   void decreaseIndent();
#ifdef NODE
#undef NODE
#endif
#define NODE(x) virtual void visit(const x &);
#include "ASTNodeTypes.def"
};


#endif /* end of include guard: PRINTER_HPP_JHX1R2VX */
