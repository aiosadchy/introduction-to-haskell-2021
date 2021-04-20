#!/usr/bin/python3

import time


MESSAGE = "Interrupted!"


def main():
    prev = time.time() - 5.0
    while True:
        try:
            time.sleep(1.0)
        except KeyboardInterrupt:
            curr = time.time()
            if (curr - prev) < 2.0:
                return
            prev = curr
            print(MESSAGE)


if __name__ == "__main__":
    main()
