// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <llvm/Support/Casting.h>

#include "AST.hpp"
#include "Visitor.hpp"

void NodeAST::setParseLocation(YYLTYPE &loc, char linebuf[500])
{
   parseLocation.first_line = loc.first_line;
   parseLocation.first_column = loc.first_column;
   parseLocation.last_line = loc.last_line;
   parseLocation.last_column = loc.last_column;

   if (loc.filename)
      parseLocation.filename = new std::string(loc.filename->c_str());
   else
      parseLocation.filename = new std::string("<unknown source>");

   parseContext = std::string(linebuf);

   parseLocationSet = true;
}

void NodeAST::compilerError(std::ostream &out, std::string msg) const
{
   if (!parseLocationSet)
      Error::exit("compilerError called on AST node without location information");

   out << ESC_CHAR << "[1m" << *parseLocation.filename << ":"
       << parseLocation.first_line << ":" << parseLocation.first_column
       << " " << ESC_CHAR << "[1;31m" << "compiler error:" << ESC_CHAR << "[0m"
       << ESC_CHAR << "[1m " << msg << ESC_CHAR << "[0m" << std::endl
       << parseContext << std::endl;

   for (unsigned int i = 1; i < parseLocation.first_column; ++i) {
      out << " ";
   }
   out << "^" << std::endl;
}

// for ignoring statements that follow return statements
void BlockAST::addStatement(StatementAST *statement)
{
   if (IsReachable) {
      statements.push_back(statement);

      if (statement->makesUnreachable())
         IsReachable = false;
   }
}

// for collapsing escaped character sequences into their character equivalents
std::string StringExpressionAST::collapseEscapedChars(const std::string &value)
{
   std::string::const_iterator it = value.begin();
   std::string result;

   while (it != value.end()) {
      if (*it == '\\') { // we have a single slash
         /// Check that we're not at the end
         if (++it == value.end()) {
            /// Append and leave!
            result += '\\';
            break;
         }
         if (*it == 'n') {
            result += '\n';
         } else if (*it == 't') {
            result += '\t';
         } else if (*it == '"') {
            result += '"';
         } else if (*it == '\\') {
            result += '\\';
         } else {
            /// We give up! Just print as-is for now!
            result += '\\';
            result += (*it);
         }
      } else {
         result += (*it);
      }
      ++it;
   }
   return result;
}

#ifdef NODE
#undef NODE
#endif
#define NODE(x) void x::accept(Visitor &v) const { v.visit(*this); }
#include "ASTNodeTypes.def"
