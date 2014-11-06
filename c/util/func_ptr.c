#include <stdio.h>


typedef void (*func_ptr)();


void hello_world()
{
	puts("Hello, World!");
}


void (*some_func())()
{
	return hello_world;
}


void (*(*func_ptr_in_func_ptr())())()
{
	return some_func;
}


int main()
{
	func_ptr my_func;
	void (*another_func)();

	my_func = hello_world;
	another_func = hello_world;

	my_func();
	another_func();
	some_func()();
	func_ptr_in_func_ptr()()();
	return 0;
}
