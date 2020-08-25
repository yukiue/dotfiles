#!/usr/bin/env python3

import argparse

from i3ipc import Connection


def parse_args():
    parser = argparse.ArgumentParser(
        description='cycle focused windows on i3 Window Manager')

    parser.add_argument('-r', '--reverse', action='store_true')
    parser.add_argument('-c', '--current', action='store_true')

    args = parser.parse_args()
    return args


def main():
    args = parse_args()

    inc = -1 if args.reverse else 1

    i3 = Connection()

    if args.current:

        focused = i3.get_tree().find_focused()

        containers = focused.workspace().descendants()

        for c in containers[:]:
            if c.name is None:
                containers.remove(c)

            index = (containers.index(focused) + inc) % len(containers)

        containers[index].command('focus')

    else:
        all_containers = i3.get_tree().leaves()

        is_focused = [c.focused for c in all_containers]

        index = (is_focused.index(True) + inc) % len(all_containers)

        all_containers[index].command('focus')


if __name__ == "__main__":
    main()
