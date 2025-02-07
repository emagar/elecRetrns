#!/usr/bin/env ruby

MAX_FILE_DELTA = 1000000
MAX_TOTAL_DELTA = 5000000

total_delta_bytes = 0
errors = []

# 'git diff-index master' lists the files in the staging area. Typically this would be run
# after a 'git add' and before 'git commit'. Then we use git-cat-file to determine the delta
# of each file from previous commit.
#
# output format of 'git diff-index master' is:
#   prev-mode new-mode prev-commit new-commit  action filename
# example:
# :000000 100644 0000000000000000000000000000000000000000 9e0f96a2a253b173cb45b41868209a5d043e1437 A	1mb.file
# :100644 100644 257cc5642cb1a054f08cc83f2d943e56fd3ebe99 9fcf2fbda9a5d884817334370d0e83e17689be7a M	README.md
IO.popen('git diff-index master').each do |l|
  _, _, prev_hash, new_hash, action, filename = l.chomp.split
  case action
  when 'A' # Added new file
    delta_bytes = %x(git cat-file -s "#{new_hash}").to_i
  when 'M' # Modified existing file
    delta_bytes = %x(git cat-file -s "#{new_hash}").to_i - %x(git cat-file -s "#{prev_hash}").to_i
  when 'D' # Deleted file
    delta_bytes = %x(git cat-file -s "#{prev_hash}").to_i
  end

  if delta_bytes > MAX_FILE_DELTA
    errors << "File '#{filename}' changed size is +#{delta_bytes} bytes. This is larger than the #{MAX_FILE_DELTA} bytes limit."
  end

  total_delta_bytes += delta_bytes
end

if total_delta_bytes > MAX_TOTAL_DELTA
  errors << "Total size of the new commit is +#{total_delta_bytes} bytes. This is larger than the #{MAX_TOTAL_DELTA} byte limit"
end

if errors.length > 0
  puts 'Commit rejected due to size of new or modified files! Errors in this commit:'
  puts
  puts errors.join("\n")
  exit(1)
end
puts "OK. Total commit size is #{total_delta_bytes} bytes."
