#include <gtk/gtk.h>

#include "subscription.h"

static void on_click(GtkWidget *button, gpointer user_data) {
	printf("%s\n", (char const *)user_data);
}

static void activate(GtkApplication *app, gpointer user_data) {
	GtkWidget *window = gtk_application_window_new(app);
	gtk_window_set_title(GTK_WINDOW(window), "Hello GTK App");
	gtk_window_set_default_size(GTK_WINDOW(window), 640, 480);

	GtkWidget *grid = gtk_grid_new();
	{
		GtkWidget *text_view = gtk_text_view_new();
		GtkTextBuffer *text_buf =
		    gtk_text_view_get_buffer(GTK_TEXT_VIEW(text_view));
		gtk_text_buffer_set_text(text_buf,
		                         "Hello, this is some text.\nMultiline.", -1);
		gtk_grid_attach(GTK_GRID(grid), text_view, 0, 0, 2, 1);
		g_signal_connect(text_buf, "changed", G_CALLBACK(on_click),
		                 "text has changed");

		GtkWidget *cancel_button = gtk_button_new_with_label("Cancel");
		g_signal_connect(cancel_button, "clicked", G_CALLBACK(on_click),
		                 "Cancel");
		gtk_grid_attach(GTK_GRID(grid), cancel_button, 0, 1, 1, 1);

		GtkWidget *ok_button = gtk_button_new_with_label("OK");
		g_signal_connect(ok_button, "clicked", G_CALLBACK(on_click), "OK");
		gtk_grid_attach(GTK_GRID(grid), ok_button, 1, 1, 1, 1);
	}
	gtk_container_add(GTK_CONTAINER(window), grid);

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
