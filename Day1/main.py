# https://adventofcode.com/2020/day/1
def filter_candidates(last_digit):
    def f(x):
        l = int(x[-1])
        if last_digit == 0:
            return l == 0
        else:
            return l == (10 - last_digit)

    return f


def main():
    with open('./input.txt') as f:
        lines = f.read().split('\n')

    for idx, i in enumerate(lines):
        last_digit = int(i[-1])
        candidates = list(filter(filter_candidates(last_digit), lines[idx + 1:]))
        i_int = int(i)
        for c in candidates:
            c_int = int(c)
            if i_int + c_int == 2020:
                print(f'Solution: {i} and {c}')
                print(f'{i} * {c} = {i_int * c_int}')
                break


if __name__ == '__main__':
    main()
