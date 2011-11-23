FloatRegex = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)|unknown/
BenchmarkLineRegex = /^([^:]+): real=(#{FloatRegex})s\s+cpu=(#{FloatRegex})s\s+gc=(#{FloatRegex})s\s*$/
require 'csv'
class BenchmarkRunner
  attr_accessor :prog_path

  def initialize(prog_path)
    self.prog_path = prog_path
  end

  def mk_cmd(benchmark_dir)
    raise "virtual!"
  end

  def run(benchmark_dir)
    result = {}
    args = mk_cmd(benchmark_dir)
    stdin_read, stdin_write = IO.pipe
    pid = Process.spawn(*args, :out => stdin_write)
    Thread.new do
      stdin_read.each_line do |line|
        if line =~ BenchmarkLineRegex
          result[File.basename($1)] = [$2, $3, $4].map{|v| v == "unknown" ? "unknown" : Float(v) }
        else
          puts "unrecognized line: #{line}"
        end
      end
    end

    rc = Process.waitpid2(pid)[1].exitstatus

    if rc != 0
      raise "command failed with exit code#{rc}: #{args.join " "}"
    end
    return result
  end
end

class RacketRunner < BenchmarkRunner
  def mk_cmd(benchmark_dir)
    %W!racket -t #{prog_path} #{benchmark_dir}!
  end
end

class PetiteRunner < BenchmarkRunner
  def mk_cmd(benchmark_dir)
    %W!petite --script #{prog_path} #{benchmark_dir}!
  end
end

class ChickenRunner < BenchmarkRunner
  def mk_cmd(benchmark_dir)
    %W!csi -ss #{prog_path} #{benchmark_dir}!
  end
end

class GambitRunner < BenchmarkRunner
  def mk_cmd(benchmark_dir)
    %W!gsi -:s #{prog_path} #{benchmark_dir}!
  end
end

class BiglooRunner < BenchmarkRunner
  def mk_cmd(benchmark_dir)
    %W!bigloo -i #{prog_path} #{benchmark_dir}!
  end
end
if __FILE__ == $0
  script_dir = File.dirname(__FILE__)
  prog_dir = File.join(script_dir, 'benchmarkers')
  benchmark_dir = File.join(script_dir, 'benchmark_sources')
  runners = [
    RacketRunner.new(File.join(prog_dir, 'racket.rkt')),
    RacketRunner.new(File.join(prog_dir, 'clinger-rees.rkt')),
    PetiteRunner.new(File.join(prog_dir, 'chez.ss')),
    ChickenRunner.new(File.join(prog_dir, 'chicken.ss')),
    GambitRunner.new(File.join(prog_dir, 'gambit.ss')),
  ]
  all_benchmarks = %W!rkt scm ss!.map {|ext| Dir.glob("#{benchmark_dir}/**/*.#{ext}") }.flatten.map {|v| File.basename(v) }
  results = runners.map {|runner| [File.basename(runner.prog_path), runner.run(benchmark_dir)]}
  CSV.open(File.join(script_dir, 'benchmarks.csv'), 'wb') do |csv|
    csv << %W!Program Benchmark Real CPU GC!
    results.each do |prog_path, result|
      #puts result.inspect
      all_benchmarks.each do |name|
        times = result.fetch(name, ['error'] * 3)
        csv << [prog_path, name] + times
      end
    end
  end
end
