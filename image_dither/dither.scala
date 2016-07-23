import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.Color


object ImageDitherer extends App {
   /** dithering: https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering **/
    println("Hello, lets's dither some image")
    val photo = ImageIO.read(new File("photo_2.jpeg"))
    val grey_photo = greyScaler(photo)
    FloydSteinberger(grey_photo)

    def rgbToGrey(red: Int, green: Int, blue: Int): Int = (red + green + blue) / 3
  
    def greyScaler(img: BufferedImage): BufferedImage = {
            val w = img.getWidth
            val h = img.getHeight
            for{
                y <- 0 to h-1
                x <- 0 to w-1
            }{
                val pixel = new Color(img.getRGB(x, y))
      		    val red =  pixel.getRed()
      		    val green = pixel.getGreen()
      		    val blue = pixel.getBlue()
      		    val grey = rgbToGrey(red, green, blue)
                img.setRGB(x, y, new Color(grey, grey, grey).getRGB)
            }
            img
    }
        

    def pixelUpdater(x: Int, y: Int, carry_val: Float, quant_error: Int, img: BufferedImage): Unit={
            var temp_rgb = new Color(img.getRGB(x , y))
            var temp_val = (temp_rgb.getRed() + (quant_error * carry_val)).toInt
            temp_val = if(temp_val>=255) 255 else temp_val
            temp_val = if(temp_val< 1) 0 else temp_val 
            img.setRGB(x , y, new Color(temp_val, temp_val, temp_val).getRGB)
    }

    def FloydSteinberger(img: BufferedImage): Unit = {
        val w = img.getWidth
        val h = img.getHeight

        for{
            /** need to use cases then add back the correct index here**/
            y <- 1 to h-2
            x <- 1 to w-2
        }{
            var old_pixel = new Color(img.getRGB(x, y))
            var grey = old_pixel.getRed()
            var new_pixel =  if(grey < 128) 0 else 255
            var quant_error = grey - new_pixel

            img.setRGB(x,y, new Color(new_pixel, new_pixel, new_pixel).getRGB)
            pixelUpdater(x+1,y, (7.toFloat/16), quant_error, img)
            pixelUpdater(x-1,y+1, (3.toFloat/16), quant_error, img)
            pixelUpdater(x,y+1, (5.toFloat/16), quant_error, img)
            pixelUpdater(x+1,y+1, (1.toFloat/16), quant_error, img)
        } 
        ImageIO.write(img, "jpg", new File("output.jpg")) 
    }
}