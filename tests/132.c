int foo(int a);

int bar(int c, int a){
	return foo(c)+foo(a);
}

int main() {
	return bar(5,2);
}

int foo(int b){
	return b+1;
}