int foo(int a,int b);

int main() {
	return foo(5,3)+foo(1,2);
}

int foo(int b, int v){
	return b+v;
}