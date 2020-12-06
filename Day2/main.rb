# https://adventofcode.com/2020/day/2
# frozen_string_literal: true

def p1_filter(i)
  token = i.match(/(\d+)-(\d+)\s(.):\s(.+)/)
  lb = Integer(token[1])
  ub = Integer(token[2])
  letter = token[3]
  password = token[4]
  password.count(letter).between?(lb, ub)
end

def p2_filter(i)
  token = i.match(/(\d+)-(\d+)\s(.):\s(.+)/)
  position_one = Integer(token[1]) - 1
  position_two = Integer(token[2]) - 1
  letter = token[3]
  password = token[4]

  p1 = password[position_one] == letter
  p2 = password[position_two] == letter

  p1 ^ p2
end

def main
  file = File.open('./input.txt')
  input = file.readlines
  file.close
  valid_passwords_p1 = input.filter { |i| p1_filter i }
  valid_passwords_p2 = input.filter { |i| p2_filter i }
  puts "Part 1 valid passwords: #{valid_passwords_p1.length}"
  puts "Part 2 valid passwords: #{valid_passwords_p2.length}"
end

main
