include module type of Simulator
module Cyclesim = Cyclesim

module For_testing : sig
  module Codegen : module type of Codegen
end
