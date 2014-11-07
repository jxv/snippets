#include <stdio.h>
#include <stdlib.h>
#include <glib.h>



void print_node(gpointer data, gpointer user_data)
{
	printf("%s", (const char *)data);
}


int main(int argc, const char **argv)
{
	GList *list = NULL;
	list = g_list_append(list, "Hello,");
	list = g_list_append(list, " ");
	list = g_list_append(list, "World!\n");

	g_list_foreach(list, print_node, NULL);

	g_list_free(list);

	return EXIT_SUCCESS;
}
