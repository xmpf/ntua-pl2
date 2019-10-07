CXX=g++
CXXFLAGS=-Wall -O2

vm : vm.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

vm_gc : vm.cpp
	$(CXX) $(CXXFLAGS) -DGC=1 $< -o $@