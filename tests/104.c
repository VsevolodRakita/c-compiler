int main() {
	int a=3;
	{
		a=5;
		int a=4;
		a=2;
	}
	return a;
}