CXX = clang
CXXFLAGS = -Wall -g

poorlog: poorlog.c
	$(CXX) $(CXXFLAGS) -o $@ $^
