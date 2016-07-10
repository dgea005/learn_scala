import scala.io.StdIn.{readLine,readInt}
import math.Pi


object DegreeConverter {
    def main(args: Array[String]): Unit = {
        val user_input = readLine("Input to convert (e.g.36dr)? ")
        val input_length = user_input.length() 
        var input_values = user_input.slice(0, input_length - 2).toFloat
        val input_unit = user_input.slice(input_length - 2, input_length - 1)
        val output_unit = user_input.slice(input_length - 1, input_length)
        println("Convert: '" + input_values + "' in '" + input_unit + "' to '" + output_unit + "' ")
        println(convert(input_unit, output_unit, input_values))

    }

    def convert(input: String, output: String, values: Float) = {
       input match {
            case "d" => convert_degrees(output, values)
            case "r" => convert_radians(output, values)
            case "f" => convert_farenheit(output, values)
            case "c" => convert_celsius(output, values)
            /**case "k" => convert_kelvin(output, values)
            **/
            case _ => "input unit not implemented"
       } 
    }

    def rounder(number: Float) = {
        Math.round(number*100.0)/100.0
    }


    def convert_degrees(output: String, values: Float) = {
        output match {
            case "r" => rounder(values * (Pi.toFloat / 180))
            case _ => "output unit '" + output + "' not implented for input 'd'"
        }
    }

    def convert_radians(output: String, values: Float) = {
        output match {
            case "d" => rounder(values * (180 / Pi.toFloat))
            case _ => "output unit '" + output + "' not implented for input 'r'"
        }
    }

    def convert_farenheit(output: String, values: Float) = {
        output match {
            case "c" => rounder((values - 32) * (5.0/9.0).toFloat)
            case "k" => rounder((values + 459.67).toFloat * (5.0/9.0).toFloat)
            case _ => "output unit '" + output + "' not implented for input 'f'"
        }
    }

    def convert_celsius(output: String, values: Float) = {
        output match {
            case "f" => rounder(values * (9.0/5.0).toFloat + 32)
            case "k" => rounder(values + 273.15.toFloat)
            case _ => "output unit '" + output + "' not implented for input 'c'"
        }
    }

    def convert_kelvin(output: String, values: Float) = {
        output match {
            case "f" => rounder(values * (9.0/5.0).toFloat - 459.67)
            case "c" => rounder(values - 273.15.toFloat)
            case _ => "output unit '" + output + "' not implented for input 'k'"
        }
    }


}