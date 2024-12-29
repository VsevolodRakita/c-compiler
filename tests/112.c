int main() {
	int c=7;
	if(c!=7){
		c=8;
		c=c+1;
		int c=5;
		c=c+10;
	}
	else{
		c=9;
		c=c+1;
		{
			int c=13;
			{
				c=14;
				int c;
			}
			c=1;
		}
	}
	return c;
}