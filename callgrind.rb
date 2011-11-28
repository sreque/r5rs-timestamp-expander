#the purpose of this script is to convert the output of the Racket Profiler's textual output to callgrind's calltree format
require 'set'
FloatRegex = /[-+]?(?:\d+(?:\.\d*)?|\.\d+)/

def create(clazz, values)
  result = clazz.new
  values.each do |k, v|
    set_sym = (k.to_s+"=").to_sym
    result.send(set_sym, v)
  end
  result
end

def hash_map(e, &blk)
  result = {}
  for v in e
    k, v = blk[v]
    result[k] = v
  end
  return result
end

class Function
  attr_accessor :name, :file, :line_no, :char_no, :index, :self_time, :total_time, :self_percent, :total_percent,
end

class CallEdge
  attr_accessor :caller_index, :callee_index, :time
end

class Node
  attr_accessor :function, :callers, :callees
end

class ProfileDataFsm
  attr_reader :nodes

  def initialize(stream)
    @input = stream
    @nodes = []
    go_idle()
    @state = :init_idle1
    parse()
    if @state == :init_idle1 || @state == :init_idle2
      $stderr.puts "Warning! Never encountered record delimiter"
    end
  end

  def parse
    @input.each_line do |line|
      @state = self.send(@state, line)
      #$stderr.puts "in state #{@state}"
    end
  end

  def go_idle()
    @callers = []
    @callees = []
    @cur_function = nil
    :idle
  end

  def init_idle1(line)
    if line =~ StartDelimiterRegex
      :init_idle2
    else
      :init_idle1
    end
  end

  def init_idle2(line)
    if line =~ StartDelimiterRegex
      :caller_or_function
    else
      :init_idle2
    end
  end

  def idle(line)
    if line =~ RecordDelimiterRegex
      :caller_or_function
    else
      unrecognized_line(line)
    end
  end

  def unrecognized_line(line)
    $stderr.puts "unrecognized line from state #{@state}: #{line}"
    go_idle()
  end

  StartDelimiterRegex = /^=========================================================================$/
  RecordDelimiterRegex = /^-------------------------------------------------------------------------$/
  FunctionNameRegex = /(?:[^\[\s]+)|(?:\[[^\]]+\])/
  TimeRegex = /(\d+)\((#{FloatRegex})%\)/ #has two captures
  FunctionRegexShared = /^\s*\[(\d+)\]\s+#{TimeRegex}\s+#{TimeRegex}\s+(#{FunctionNameRegex})\s+/
  FunctionRegex = /#{FunctionRegexShared}([^:]+):(?:(\d+):(\d+)|##f)$/
  UnknownSourceRegex = /#{FunctionRegexShared}\(unknown source\)$/
  CallerCalleeRegex = /^\s+(#{FunctionNameRegex}) \[(\d+)\]\s*(#{FloatRegex})%/
  Relationship = Struct.new(:name, :index, :time_percent)

  def caller_or_function(line)
    if line =~ CallerCalleeRegex
      @callers << Relationship.new($1, Integer($2), Float($3) / 100)
      :caller_or_function
    elsif line =~ FunctionRegex or line =~ UnknownSourceRegex
      file = $7.nil? ? "<unknown>" : $7
      line_no, char_no = [$8, $9].map {|v| v.nil? ? "-1" : Integer(v) } 
      @cur_function = create(Function, 
                             :index => Integer($1),
                             :total_time => Integer($2),
                             :total_percent => Float($3) / 100,
                             :self_time => Integer($4),
                             :self_percent => Float($5) / 100,
                             :name => $6,
                             :file => file,
                             :line_no => line_no, 
                             :char_no => char_no)
      :callee
    else
      unrecognized_line(line)
    end
  end

  def callee(line)
    if line =~ CallerCalleeRegex
      @callees << Relationship.new($1, Integer($2), Float($3) / 100)
      :callee
    elsif line =~ RecordDelimiterRegex
      finish_record()
    else
      unrecognized_line(line)
    end
  end
  
  def finish_record()
    if @callers.empty? && @callees.empty?
      raise "callers and callees list should not both be empty!"
    end
    raise "no function found" if  @cur_function.nil?
    callers = @callers.map() {|v| create(CallEdge, :caller_index => v.index, :callee_index => @cur_function.index, :time => @cur_function.total_time * v.time_percent) } #the time on these is meanignless
    callees = @callees.map() {|v| create(CallEdge, :caller_index => @cur_function.index, :callee_index => v.index, :time => @cur_function.total_time * v.time_percent) }
    @nodes << create(Node, :function => @cur_function, :callers => callers, :callees => callees)
    go_idle()
    :caller_or_function
  end
end

if __FILE__ == $0
  nodes = ProfileDataFsm.new($stdin).nodes
  by_index = hash_map(nodes) {|n| [n.function.index, n] }
  files_to_index = Set.new(nodes.map{|v| v.function.file }).inject({}) {|h, f| h[f] = h.size; h }

  puts "events: Instructions"

  for k, v in files_to_index
    puts "fl=(#{v}) #{k}"
  end

  for node in nodes
    puts "fn=(#{node.function.index}) #{node.function.name}[#{node.function.index}]"
  end

  instr_scale_factor = 1000
  for node in nodes
    f = node.function
    puts "fl=(#{files_to_index.fetch(f.file)})"
    puts "fn=(#{f.index})"
    puts "#{f.line_no} #{(f.self_time * instr_scale_factor).round}"
    for c in node.callees
      called_node = by_index.fetch(c.callee_index)
      if called_node.function.file != f.file
        puts "cfl=(#{files_to_index.fetch(called_node.function.file)})"
      end
      raise "uh oh! #{c.callee_index} != #{called_node.function.index}" if c.callee_index != called_node.function.index
      puts "cfn=(#{c.callee_index})"
      puts "calls=1 #{called_node.function.line_no}"
      puts "#{f.line_no} #{(c.time * instr_scale_factor).round}"
    end
  end
end
