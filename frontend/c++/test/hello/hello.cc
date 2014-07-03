#include <iostream>

int main(int argc, char **argv)
{
	unsigned long addr = reinterpret_cast<unsigned long>(&argc);
	std::cerr << "Address of argc as an integer is " 
		<< std::hex << addr
		<< std::dec << std::endl;
	std::cerr << "Back to pointer: " 
		<< reinterpret_cast<int *>(addr)
		<< std::endl;

	return 0;
}
