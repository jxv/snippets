#include <stdio.h>

void get_line(char *str, int size)
{
	int i;
	char c;

	for (i = 0; i < size - 1; i++) {
		c = getchar();
		str[i] = c;
		if (c == '\n') {
			str[i] = '\0';
			break;
		}
	}
	str[size - 1] = '\0';
}

int main()
{
	char line[256];
	get_line(line, 256);
	printf("%s\n", line);
	return 0;
}
