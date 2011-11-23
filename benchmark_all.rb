FloatRegex = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)|unknown/
BenchmarkLineRegex = /^([^:]+): real=(#{FloatRegex})s\s+cpu=(#{FloatRegex})s\s+gc=(#{FloatRegex})s\s*$/
require 'csv'

def run_process_get_output(args, out_type=:out, &blk)
  stdin_read, stdin_write = IO.pipe
  pid = Process.spawn(*args, out_type => stdin_write)
  result = [nil]
  t = Thread.new do
    result[0] = blk[stdin_read]
  end

  rc = Process.waitpid2(pid)[1].exitstatus
  stdin_write.close
  t.join
  stdin_read.close

  if rc != 0
    raise "command failed with exit code#{rc}: #{args.join " "}"
  end
  return result[0]
end

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
    run_process_get_output(args) do |stdin_read|
      stdin_read.each_line do |line|
        if line =~ BenchmarkLineRegex
          result[File.basename($1)] = [$2, $3, $4].map{|v| v == "unknown" ? "unknown" : Float(v) }
        else
          puts "unrecognized line: #{line}"
        end
      end
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

class IkarusRunner < BenchmarkRunner
  def mk_cmd(benchmark_dir)
    %W!ikarus --r6rs-script #{prog_path} #{benchmark_dir}!
  end

  def run(benchmark_dir)
    args = mk_cmd(benchmark_dir)
    run_process_get_output(args, :err) do |stdin_read|
      IkarusFsm.new(stdin_read).results
    end
  end
end

class IkarusFsm
  attr_accessor :results
  def initialize(input)
    @input = input
    self.results = {}
    @next_func = :idle
    parse()
  end

  def parse()
    @input.each_line do |line|
      @next_func = self.send(@next_func, line)
    end
  end

  def idle(line)
    if line =~ /^running stats for `([^`]+)`:\s*$/
      @file, @real, @cpu, @gc = $1, nil, nil, nil
       :expect_collections
    else
      unrecognized(line)
    end
  end

  def expect_collections(line)
    if line =~ /^    (?:\d+|no) collections/
      :expect_cpu
    else
      unrecognized(line)
    end
  end

  def expect_cpu(line)
    if line =~ /^    (\d+) ms elapsed cpu time, including (\d+) ms collecting$/
      @cpu = Float($1) / 1000
      @gc = Float($2) / 1000
      :expect_real
    else
      unrecognized(line)
    end
  end

  def expect_real(line)
    if line =~ /^    (\d+) ms elapsed real time, including (\d+) ms collecting$/
      @real = Float($1) / 1000
      @gc = Float($2) / 1000
      :expect_allocated
    else
      unrecognized(line)
    end
  end

  def expect_allocated(line)
    if line =~ /^    (\d+) bytes allocated$/
      self.results[File.basename(@file)] = [@real, @cpu, @gc]
      go_idle
    else
      unrecognized(line)
    end
  end

  def unrecognized(line)
    puts "unrecognized line: #{line}"
    go_idle
  end

  def go_idle
    @file, @real, @cpu, @gc = nil, nil, nil, nil
    return :idle
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
    IkarusRunner.new(File.join(prog_dir, 'ikarus.ss')),
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
