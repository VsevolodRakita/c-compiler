int main() {
	int a=0;
	for(int i=1; i<7;i=i+1){
		if (i%4==0)
			break;
		a=a+1;
	}
	return a;
}