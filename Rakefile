require 'fileutils'
require 'erb'

task :default => :install

desc "install the dotfiles in the user's home directory"
task :install do
  SUPPORT = ['Rakefile', 'README', 'LICENSE']
  replace_all = false

  Dir['*'].reject {|f| SUPPORT.include?(f) || File.directory?(f) }.each do |file|
    erb = file.sub!(/\.erb$/, '') ? true : false

    if File.exist? dot(file) or File.symlink? dot(file)
      if replace_all
        replace file, erb
      else
        msg "exists", :yellow, file, "-- replace? <yes> (n)o (a)ll "
        case $stdin.gets.chomp
          when 'a'
            replace_all = true
            replace file, erb
          when 'n'
            msg "skipping", :cyan, file, "\n"
          when 'q'
            exit
          else
            replace file, erb
        end
      end
    else
      link file, erb
    end
  end
end

def replace file, erb
  FileUtils.rm dot(file)
  link file, erb
end

def link file, erb
  if erb
    msg "generating", :green, file, "\n"
    File.open(dot(file), 'w') do |f|
      f.write ERB.new(File.read(file + '.erb')).result(binding)
    end
  else
    msg "linking", :green, file, "\n"
    FileUtils.symlink File.join(ENV['PWD'], file), dot(file)
  end
end

def dot file
  File.join(ENV['HOME'], '.' + file)
end

def msg title, style, file = nil, str = ""
  msg = title
  msg += " ~/.#{file} " + str
  print msg
end

