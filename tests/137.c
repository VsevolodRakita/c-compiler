int foo(int a1,int a2,int a3,int a4,int a5,int a6,int a7,int a8);

int bar(int a1,int a2,int a3,int a4,int a5,int a6,int a7){
	return a1+a7;
}
int main() {
	int a1=1;
	int a6=2;
	int b=foo(3,4,5,6,7,8,9,10);
	int a8=3;
	return b+bar(11,0,0,12,0,0,14);
}

int foo(int a1,int a2,int a3,int a4,int a5,int a6,int a7,int a8){
	if (a1<3){
		return 1;
	}
	if (a6<5){
		return 2;
	}
	return a4+bar(7,0,0,0,0,0,8);
}