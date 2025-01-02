int main() {
	int a=0;
	int i=0;
	while(a<50){
		i=i+1;
		if(i%2==0)
			continue;
		a=a+10;
		
	}
	return a-i;
}