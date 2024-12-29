int main() {
	int a=3;
	{
		int a=4;
		{
			int a=5;
		}
		a=2;
	}
	return a;
}