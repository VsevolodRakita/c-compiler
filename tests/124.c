int main() {
	int a=0;
	do {
		a=a+11;
		if (a%23==0)
			continue;
		a=a+1;
	}while(a<30);
	return a;
}