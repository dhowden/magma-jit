// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#ifndef ERROR_HPP_O6PTNL8L
#define ERROR_HPP_O6PTNL8L

#include <iostream>

class NodeAST;

//===----------------------------------------------------------------------===//
// Error handling class
//===----------------------------------------------------------------------===//

class Error {
public:
   static void exit(const NodeAST *, std::string);
   static void exit(const NodeAST &, std::string);
   static void exit(std::string);
};

#endif /* end of include guard: ERROR_HPP_O6PTNL8L */
