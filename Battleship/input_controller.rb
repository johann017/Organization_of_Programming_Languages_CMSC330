require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# reourn a px_row && column <= @max_column)opulated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
    begin
      gameboard = GameBoard.new 10,10 
      counter = 0
      read_file_lines(path) do |line|
        if counter < 5 then
          if line =~ /^\((\d+),(\d+)\), (.+), (\d)$/ then
            row = $1.to_i
            column = $2.to_i
            orientation = $3
            size = $4.to_i
            if (row >= 1 && column >= 1 && row <= gameboard.max_row && column <= gameboard.max_column) && (["Up", "Down", "Left", "Right"].include? orientation) && (size >= 1 && size <= 5) then
              if gameboard.add_ship(Ship.new(Position.new(row,column), orientation, size)) then
                counter += 1
              end
            end
          end
        end
      end
    rescue Exception => e
      puts "Exception:" + e.to_s + " (class " + e.class.to_s + ")"
      return nil
    end
    if counter != 5 then
      return nil
    end
    return gameboard
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    begin
      arr = Array.new
      read_file_lines(path) do |line|
        if line =~ /^\((\d+),(\d+)\)$/ then
          row = $1.to_i
          column = $2.to_i
          arr.push(Position.new(row,column))
        end
      end
    rescue Exception => e
      puts "Exception:" + e.to_s + " (class " + e.class.to_s + ")"
      return nil
    end
  return arr
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
