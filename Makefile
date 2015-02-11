all: mj

OBJS = Parser.o           \
       main.o             \
       Tokens.o           \
       Printer.o          \
       AST.o              \
       Error.o            \
       CodeGen.o          \
       ContextLookAhead.o \
       MagmaType.o

LLVM_MODULES = core jit native

CXX=clang++ -g
# Remove -O3 for debugging
# CXXFLAGS=$(subst -O3,,$(shell llvm-config --cxxflags))
CXXFLAGS=`llvm-config --cxxflags`
LDFLAGS=`llvm-config --ldflags`
LIBS=`llvm-config --system-libs --libs $(LLVM_MODULES)`
BISON=bison
FLEX=flex

clean:
	$(RM) -rf mj Parser.cpp Parser.hpp Tokens.cpp $(OBJS)

Parser.cpp: Parser.y
	$(BISON) -d -o $@ $^

Parser.hpp: Parser.cpp

Tokens.cpp: Tokens.l Parser.hpp
	$(FLEX) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $(CPPFLAGS) -o $@ $<

mj: $(OBJS)
	$(CXX) -o $@ $(LDFLAGS) $(OBJS) $(LIBS)
