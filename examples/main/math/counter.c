#include <stdio.h>

int main(void) {
	for (int i = 0; i < 10000000; i++) {
		if (i % 1000000 == 0) {
			printf("%d\n", i);
		}
	}

	printf("Done");

	return 0;
}