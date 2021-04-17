#include <gtk/gtk.h>
#include <stdbool.h>

#include "subscription.h"

// シグナルのハンドラ。イベントが発生したとき (シグナルしたとき) に呼ばれる。
static void on_signal(GtkWidget *widget, gpointer data) {
	printf("%s\n", (char const *)data);
}

static void activate(GtkApplication *app, gpointer user_data) {
	GtkWidget *window = gtk_application_window_new(app);
	gtk_window_set_title(GTK_WINDOW(window), "Tiny GTK Notepad");
	gtk_window_set_default_size(GTK_WINDOW(window), 640, 480);

	// padding: 0, これは要素間につくっぽい。要素を追加するときにつけるマージンと重なるので0にしている。
	GtkWidget *box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	{
		GtkWidget *text_view = gtk_text_view_new();
		GtkTextBuffer *text_buf =
		    gtk_text_view_get_buffer(GTK_TEXT_VIEW(text_view));
		gtk_text_buffer_set_text(
		    text_buf,
		    "Hello, this is tiny notepad built with GTK "
		    "app.\n日本語入力もできる。\n絵文字は表示されないっぽい: 😄.\n",
		    -1);
		g_signal_connect(text_buf, "changed", G_CALLBACK(on_signal),
		                 "text has changed");
		// expand, fill: true. ボックスに割り当てられた領域をこれで埋める。
		// padding: 0. 要素の前後につくっぽい。
		gtk_box_pack_start(GTK_BOX(box), text_view, true, true, 0);

		GtkWidget *button_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
		gtk_widget_set_halign(button_box, GTK_ALIGN_END); // 右寄せ
		// gtk_widget_set_margin_end(button_box, 5); // 右側にマージンを入れる
		{
			GtkWidget *open_button = gtk_button_new_with_label("開く");
			g_signal_connect(open_button, "clicked", G_CALLBACK(on_signal),
			                 "開くボタンが押された");
			gtk_box_pack_start(GTK_BOX(button_box), open_button, false, false,
			                   0);

			GtkWidget *save_button = gtk_button_new_with_label("保存");
			g_signal_connect(save_button, "clicked", G_CALLBACK(on_signal),
			                 "保存ボタンが押された");
			gtk_box_pack_start(GTK_BOX(button_box), save_button, false, false,
			                   5);
		}
		gtk_box_pack_start(GTK_BOX(box), button_box, false, false, 5);
	}
	gtk_container_add(GTK_CONTAINER(window), box);

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
