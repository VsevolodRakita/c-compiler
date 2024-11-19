int main() {
	int a;
	int b=3;
	a=b+1;
	int c=a+b;
	if(a<c-b)
		return 5;
	else if(a>c-b)
		return 6;
	return 7;
}