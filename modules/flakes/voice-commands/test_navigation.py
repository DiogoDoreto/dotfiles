"""Full targeting traces against the production navigation interface, without Qt."""

import unittest

from navigation import GridNavigation, Monitor
from vocabulary import NUMBERS


class NavigationTests(unittest.TestCase):
    def assert_region(self, navigation, expected):
        region = navigation.view.region
        for actual, value in zip((region.x, region.y, region.width, region.height), expected):
            self.assertAlmostEqual(actual, value)

    def assert_target(self, navigation, expected):
        result = navigation.command("click")
        self.assertEqual(result.state, "click")
        self.assertFalse(result.stage_changed)
        for actual, value in zip(result.position, expected):
            self.assertAlmostEqual(actual, value)

    def test_single_monitor_trace_back_reset_and_target(self):
        navigation = GridNavigation([Monitor("portrait", -400, 120, 900, 1620)])
        self.assertEqual(navigation.view.stage, "grid")
        self.assertEqual(navigation.view.monitor, 1)
        self.assert_region(navigation, (0, 0, 900, 1620))
        root = navigation.view
        self.assertEqual(navigation.command("three").state, "grid")
        self.assertFalse(navigation.command("seven").stage_changed)
        self.assertEqual(navigation.view.path, (3, 7))
        self.assert_region(navigation, (600, 360, 100, 180))
        self.assert_target(navigation, (250, 570))
        navigation.command("back")
        self.assertEqual(navigation.view.path, (3,))
        self.assert_region(navigation, (600, 0, 300, 540))
        navigation.command("nine")
        navigation.command("reset")
        self.assertEqual(navigation.view, root)
        self.assertFalse(navigation.command("back").stage_changed)
        self.assertEqual(navigation.view, root)
        self.assert_target(navigation, (50, 930))
        self.assertEqual(root.path, (), "Previously published views must remain immutable")

    def test_multimonitor_trace_unequal_sizes_and_negative_vertical_origins(self):
        right = Monitor("lower", 0, 900, 1440, 900)
        left = Monitor("left", -1920, -300, 1920, 1080)
        upper = Monitor("upper", 0, -900, 2560, 1440)
        navigation = GridNavigation([right, upper, left])
        self.assertEqual(navigation.view.monitors, (left, upper, right))
        self.assertEqual(navigation.view.stage, "monitors")
        choice = navigation.view
        for command in ("click", "nine", "zero", "back", "reset"):
            result = navigation.command(command)
            self.assertIsNone(result.position)
            self.assertFalse(result.stage_changed)
            self.assertEqual(navigation.view, choice)
        for number, size, trace, target in [
                ("one", (1920, 1080), ("three", "seven"), (-1600 / 3, 0)),
                ("two", (2560, 1440), ("nine", "five"), (6400 / 3, 300)),
                ("three", (1440, 900), ("one", "nine"), (400, 1150))]:
            with self.subTest(monitor=number):
                result = navigation.command(number)
                self.assertEqual(result.state, "grid")
                self.assertTrue(result.stage_changed)
                self.assertEqual(navigation.view.path, (), "Choosing a monitor must not refine")
                self.assert_region(navigation, (0, 0, *size))
                for command in trace:
                    self.assertFalse(navigation.command(command).stage_changed)
                self.assert_target(navigation, target)
                navigation.command("back")
                self.assertEqual(len(navigation.view.path), 1)
                navigation.command("reset")
                self.assertEqual(navigation.view.path, ())
                self.assert_region(navigation, (0, 0, *size))
                result = navigation.command("back")
                self.assertEqual(result.state, "monitors")
                self.assertTrue(result.stage_changed)
                self.assertEqual(navigation.view, choice)

    def test_monitor_order_uses_x_then_y_then_name(self):
        monitors = [Monitor("Z", 0, 0, 900, 600), Monitor("A", 0, 0, 1200, 900),
                    Monitor("above", 0, -600, 600, 600), Monitor("left", -900, 900, 900, 600)]
        for layout in (monitors, list(reversed(monitors))):
            navigation = GridNavigation(layout)
            self.assertEqual([monitor.name for monitor in navigation.view.monitors], ["left", "above", "A", "Z"])
            navigation.command("three")
            self.assert_region(navigation, (0, 0, 1200, 900))
            self.assert_target(navigation, (600, 450))

    def test_all_nine_cells_are_row_major(self):
        navigation = GridNavigation([Monitor("wide", -300, 600, 900, 540)])
        expected = [(-150, 690), (150, 690), (450, 690),
                    (-150, 870), (150, 870), (450, 870),
                    (-150, 1050), (150, 1050), (450, 1050)]
        for number, target in zip(NUMBERS, expected):
            with self.subTest(number=number):
                navigation.command(number)
                self.assert_target(navigation, target)
                navigation.command("reset")

    def test_precision_floor_rejection_back_and_reset(self):
        navigation = GridNavigation([Monitor("small", -100, 200, 81, 27)])
        for unused in range(3):
            navigation.command("nine")
        self.assert_region(navigation, (78, 26, 3, 1))
        deepest = navigation.view
        for unused in range(20):
            result = navigation.command("one")
            self.assertEqual(result.state, "limit")
            self.assertFalse(result.stage_changed)
            self.assertEqual(navigation.view, deepest)
        self.assert_target(navigation, (-20.5, 226.5))
        navigation.command("back")
        navigation.command("one")
        self.assert_region(navigation, (72, 24, 3, 1))
        navigation.command("reset")
        self.assert_region(navigation, (0, 0, 81, 27))
        self.assertEqual(navigation.view.path, ())

    def test_precision_floor_uses_smaller_logical_dimension(self):
        for width, height, refinements in [(1, 2000, 0), (2000, 1, 0), (10, 7, 1)]:
            with self.subTest(size=(width, height)):
                navigation = GridNavigation([Monitor("logical", 0, 0, width, height)])
                for unused in range(refinements):
                    self.assertEqual(navigation.command("five").state, "grid")
                view = navigation.view
                self.assertEqual(navigation.command("five").state, "limit")
                self.assertEqual(navigation.view, view)
                self.assertGreaterEqual(min(view.region.width, view.region.height), 1)
                self.assert_target(navigation, (width / 2, height / 2))

    def test_invalid_commands_cancel_intent_and_close(self):
        navigation = GridNavigation([Monitor("one", 0, 0, 900, 600), Monitor("two", 900, 0, 600, 900)])
        view = navigation.view
        self.assertEqual(navigation.command("nine").state, "rejected")
        self.assertEqual(navigation.command("click").state, "select-monitor")
        self.assertEqual(navigation.command("cancel").state, "cancel")
        self.assertEqual(navigation.view, view, "Lifetime owner applies terminal intents")
        navigation.command("two")
        navigation.command("five")
        view = navigation.view
        for command in ("zero", "ten", "1", "one two", "scroll", ""):
            self.assertIsNone(navigation.command(command).state)
            self.assertEqual(navigation.view, view)
        self.assertEqual(navigation.command("cancel").state, "cancel")
        navigation.close()
        self.assertIsNone(navigation.view.stage)
        self.assertIsNone(navigation.view.region)
        self.assertIsNone(navigation.view.monitor)
        self.assertEqual(navigation.view.monitors, ())
        self.assertEqual(navigation.view.path, ())
        for command in ("click", "one", "back", "reset", "cancel"):
            self.assertIsNone(navigation.command(command).state)
        navigation.close()

    def test_monitor_count_and_geometry_validation(self):
        for monitors in ([], [Monitor(str(n), n * 900, 0, 900, 600) for n in range(10)],
                         [Monitor("empty", 0, 0, 0, 600)], [Monitor("negative", 0, 0, 900, -1)]):
            with self.subTest(monitors=monitors), self.assertRaises(ValueError):
                GridNavigation(monitors)
        navigation = GridNavigation([Monitor(str(n), n * 900, 0, 900, 600) for n in range(9)])
        navigation.command("nine")
        self.assertEqual(navigation.view.monitor, 9)
        self.assert_target(navigation, (7650, 300))


if __name__ == "__main__":
    unittest.main()
