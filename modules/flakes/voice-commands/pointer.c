/* Plasma-only absolute pointer input in the same logical coordinates as the overlay. */
#include <errno.h>
#include <linux/input-event-codes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wayland-client.h>
#include "fake-input-client-protocol.h"

static struct org_kde_kwin_fake_input *input;

static void global(void *data, struct wl_registry *registry, uint32_t name,
                   const char *interface, uint32_t version)
{
    (void)data;
    if (!strcmp(interface, "org_kde_kwin_fake_input") && version >= 3) {
        input = wl_registry_bind(registry, name, &org_kde_kwin_fake_input_interface,
                                 version < 5 ? version : 5);
    }
}

static void removed(void *data, struct wl_registry *registry, uint32_t name)
{
    (void)data;
    (void)registry;
    (void)name;
}

int main(int argc, char **argv)
{
    int check = argc == 2 && !strcmp(argv[1], "--check");
    double position[2] = {0, 0};
    if (!check) {
        if (argc != 3) {
            fprintf(stderr, "Usage: voice-commands-pointer X Y | --check\n");
            return 1;
        }
        for (int i = 0; i < 2; i++) {
            char *end;
            errno = 0;
            position[i] = strtod(argv[i + 1], &end);
            if (errno || end == argv[i + 1] || *end || !isfinite(position[i]) ||
                fabs(position[i]) > 8388607) {
                fprintf(stderr, "Invalid logical coordinate\n");
                return 1;
            }
        }
    }
    /* Bound registry discovery and compositor roundtrips even if KWin stops responding. */
    alarm(3);
    struct wl_display *display = wl_display_connect(NULL);
    if (!display) {
        fprintf(stderr, "Could not connect to the Wayland desktop\n");
        return 1;
    }
    struct wl_registry *registry = wl_display_get_registry(display);
    const struct wl_registry_listener listener = {.global = global, .global_remove = removed};
    wl_registry_add_listener(registry, &listener, NULL);
    if (wl_display_roundtrip(display) < 0 || !input) {
        fprintf(stderr, "KWin absolute-input interface is unavailable; install the Voice Commands desktop registration in this session\n");
        wl_display_disconnect(display);
        return 1;
    }
    if (!check) {
        org_kde_kwin_fake_input_authenticate(input, "Voice Commands", "Voice mouse-grid click");
        if (wl_display_roundtrip(display) < 0) {
            wl_display_disconnect(display);
            return 1;
        }
        /* A desktop without a physical mouse must first bind the new seat capability. */
        usleep(100000);
        org_kde_kwin_fake_input_pointer_motion_absolute(input,
            wl_fixed_from_double(position[0]), wl_fixed_from_double(position[1]));
        org_kde_kwin_fake_input_button(input, BTN_LEFT, WL_POINTER_BUTTON_STATE_PRESSED);
        org_kde_kwin_fake_input_button(input, BTN_LEFT, WL_POINTER_BUTTON_STATE_RELEASED);
    }
    int result = wl_display_roundtrip(display);
    if (!check) {
        /* Let clients consume the input before the virtual device is removed. */
        usleep(50000);
    }
    wl_display_disconnect(display);
    if (result < 0) {
        fprintf(stderr, "KWin input request failed\n");
        return 1;
    }
    return 0;
}
