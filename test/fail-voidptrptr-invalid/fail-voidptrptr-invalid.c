static void swap_ptrs(void **p1, void **p2)
{
	void *temp = *p1;
	*p1 = *p2;
	*p2 = temp;
}

int main(void)
{
	/* Here we do stuff that's genuinely invalid. Example: 1
	 * use our swap function to swap two unsigned longs. */
	int c = 42;
	int d = 69105;
	unsigned long long blah = (unsigned long long) &c;
	unsigned long long foo = (unsigned long long) &d;
	swap_ptrs(&blah, &foo);

	/* Any more? */

	return 0;
}
