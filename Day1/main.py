# https://adventofcode.com/2020/day/1

# def filter_candidates(last_digit):
#     def f(x):
#         l = int(x[-1])
#         if last_digit == 0:
#             return l == 0
#         else:
#             return l == (10 - last_digit)
#
#     return f
# last_digit = int(i[-1])
# candidates = list(filter(filter_candidates(last_digit), lines[idx + 1:]))


def main():
    with open('./input.txt') as f:
        lines = f.read().split('\n')

    for idx, i in enumerate(lines):
        i_int = int(i)
        for s_idx, s in enumerate(lines[idx + 1:]):
            s_int = int(s)
            if i_int + s_int == 2020:
                print(f'Solution for 2 numbers found: {i} and {s}')
                print(f'{i} * {s} = {i_int * s_int}\n')
            elif i_int + s_int < 2020:
                for t in lines[s_idx + 1:]:
                    t_int = int(t)
                    if i_int + s_int + t_int == 2020:
                        print(f'Solution for 3 numbers found: {i} and {s} and {t}')
                        print(f'{i} * {s} * {t} = {i_int * s_int * t_int}\n')


if __name__ == '__main__':
    main()
