int fibonacci(int a);

int main() {
	return fibonacci(6);
}

int fibonacci(int a){
	if(a==0 || a==1){
		return a;
	}
	return fibonacci(a-1)+fibonacci(a-2);
}