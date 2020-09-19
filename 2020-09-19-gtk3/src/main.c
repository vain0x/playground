#include <gtk/gtk.h>

#include "subscription.h"

static void activate(GtkApplication *app, gpointer user_data) {
	GtkWidget *window = gtk_application_window_new(app);
	gtk_window_set_title(GTK_WINDOW(window), "Hello GTK App");
	gtk_window_set_default_size(GTK_WINDOW(window), 640, 480);
	gtk_widget_show_all(window);
}

int main(int argc, char **argv) {
	GtkApplication *app =
	    gtk_application_new("org.gtk.example", G_APPLICATION_FLAGS_NONE);
	g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);

	int status = g_application_run(G_APPLICATION(app), argc, argv);
	g_object_unref(app);

	return status;
}
