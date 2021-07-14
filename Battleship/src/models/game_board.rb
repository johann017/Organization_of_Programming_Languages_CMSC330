class GameBoard
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
        @array = Array.new(max_row) {Array.new(max_column)}
        @num_success = 0
        @num_ships = 0
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
        x = ship.start_position.row
        y = ship.start_position.column
        if ship.orientation.eql? "Right" then
          y = ship.start_position.column + ship.size - 1
          if x <= @max_row && x >= 1 && y <= @max_column && y >= 1 then
            return add_ship_helper(ship,x,y,"y")
          end
        end
        if ship.orientation.eql? "Left" then
          y = ship.start_position.column - ship.size + 1
          if x <= @max_row && x >= 1 && y <= @max_column && y >= 1 then
            return add_ship_helper(ship,x,y,"y")
          end
        end
        if ship.orientation.eql? "Up" then
          x = ship.start_position.row - ship.size + 1
          if x <= @max_row && x >= 1 && y <= @max_column && y >= 1 then
            return add_ship_helper(ship,x,y,"x")
          end
        end
        if ship.orientation.eql? "Down" then
          x = ship.start_position.row + ship.size - 1
          if x <= @max_row && x >= 1 && y <= @max_column && y >= 1 then
            return add_ship_helper(ship,x,y,"x")
          end
        end
        return false    
    end

    def add_ship_helper(ship,x, y, z)
      if (z.eql? "x") then
        if ship.start_position.row < x then 
          for i in ship.start_position.row..x
            if @array[i-1][ship.start_position.column-1] then
              return false
            end
          end
          for i in ship.start_position.row..x
            @array[i-1][ship.start_position.column-1] = "B"
          end
        else
          for i in x..ship.start_position.row
            if @array[i-1][ship.start_position.column-1] then
              return false
            end
          end
          for i in x..ship.start_position.row
            @array[i-1][ship.start_position.column-1] = "B"
          end
        end
      end
      if (z.eql? "y") then
        if ship.start_position.column < y then
          for i in ship.start_position.column..y
            if @array[ship.start_position.row-1][i-1] then
              return false
            end
          end
          for i in ship.start_position.column..y
            @array[ship.start_position.row-1][i-1] = "B"
          end
        else
          for i in y..ship.start_position.column
            if @array[ship.start_position.row-1][i-1] then
              return false
            end
          end
          for i in y..ship.start_position.column
            @array[ship.start_position.row-1][i-1] = "B"
          end
        end
      end
      @num_ships += 1
      return true
    end
    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
    def attack_pos(position)
      if position.row <= @max_row && position.row >= 1 && position.column <= @max_column && position.column >= 1 then
        if (@array[position.row-1][position.column-1].eql? "B") then
          @array[position.row-1][position.column-1] = "H"
          @num_success += 1
          return true
        elsif (@array[position.row-1][position.column-1].eql? "H") then
          return true
        else
          @array[position.row-1][position.column-1] = "A"
          return false
        end
      end
      return nil
    end

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @num_success
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
      if @num_ships != 0 then
        for i in 0...@max_row
          for j in 0...@max_column
            if (@array[i][j].eql? "B") && !(@array[i][j].eql? "H") then
              return false
            end
          end
        end
      end
      return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
      @array.each {|i| puts i.to_s}
    end
end
