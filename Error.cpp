// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "Error.hpp"
#include "AST.hpp"

void Error::exit(const NodeAST *node, std::string errorMessage)
{
   Error::exit(*(node), errorMessage);
}

void Error::exit(const NodeAST &node, std::string errorMessage)
{
   node.compilerError(std::cerr, errorMessage);
   std::exit(1);
}

void Error::exit(std::string errorMessage)
{
   std::cerr << "Compiler Error: " << errorMessage << std::endl;
   std::exit(1);
}
