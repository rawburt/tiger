TESTCASE_DIR = "./testcases"
TESTS = Dir.entries(TESTCASE_DIR)

def output_failure(testfile, output, first_line)
  puts "FAILURE =================== [ #{testfile} ]"
  # if the first line is a comment then it is informative
  puts first_line if first_line =~ /^\/\*/
  puts output
  puts
end

count = 0
failures = 0

TESTS.each do |testfile|
  next unless testfile =~ /\.tig/

  count += 1

  fullpath = TESTCASE_DIR + "/#{testfile}"
  # if an error is expected, the first line of the file contains "/* error"
  first_line = File.open(fullpath, &:readline)
  expects_error = first_line =~ /^\/\* error/

  # capture STDOUT
  output = `dune exec tiger #{fullpath}`

  if $?.success?
    # the program was successful -- check if that's OK
    if expects_error
      output_failure(testfile, output, first_line)
      failures += 1
    end
  else
    # the program had an error -- check if that's OK
    if !expects_error
      output_failure(testfile, output, first_line)
      failures += 1
    end
  end
end

puts
puts "----------------------[ RESULTS ]----------------------"
puts "tests: #{count}"
puts "failures: #{failures}"
puts
