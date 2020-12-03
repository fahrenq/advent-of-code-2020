# https://adventofcode.com/2020/day/2
# frozen_string_literal: true

def main
  file = File.open('./input.txt')
  input = file.readlines
  file.close
  valid_passwords = input.filter do |i|
    token = i.match(/(\d+)-(\d+)\s(.):\s(.+)/)
    lb = Integer(token[1])
    ub = Integer(token[2])
    letter = token[3]
    password = token[4]
    password.count(letter).between?(lb, ub)
  end
  puts valid_passwords.length
end

main
