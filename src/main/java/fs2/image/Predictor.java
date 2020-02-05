package fs2.pdf.image;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

/**
 * Helper class to contain predictor decoding used by Flate and LZW filter.
 * To see the history, look at the FlateFilter class.
 */
public final class Predictor
{

    private Predictor()
    {
    }

    /**
     * Decodes a single line of data in-place.
     * @param predictor Predictor value for the current line
     * @param colors Number of color components, from decode parameters.
     * @param bitsPerComponent Number of bits per components, from decode parameters.
     * @param columns Number samples in a row, from decode parameters.
     * @param actline Current (active) line to decode. Data will be decoded in-place,
     *                i.e. - the contents of this buffer will be modified.
     * @param lastline The previous decoded line. When decoding the first line, this
     *                 parameter should be an empty byte array of the same length as
     *                 <code>actline</code>.
     */
    public static void decodePredictorRow(int predictor, int colors, int bitsPerComponent, int columns, byte[] actline, byte[] lastline)
    {
        if (predictor == 1)
        {
            // no prediction
            return;
        }
        final int bitsPerPixel = colors * bitsPerComponent;
        final int bytesPerPixel = (bitsPerPixel + 7) / 8;
        final int rowlength = actline.length;
        switch (predictor)
        {
            case 2:
                // PRED TIFF SUB
                if (bitsPerComponent == 8)
                {
                    // for 8 bits per component it is the same algorithm as PRED SUB of PNG format
                    for (int p = bytesPerPixel; p < rowlength; p++)
                    {
                        int sub = actline[p] & 0xff;
                        int left = actline[p - bytesPerPixel] & 0xff;
                        actline[p] = (byte) (sub + left);
                    }
                    break;
                }
                if (bitsPerComponent == 16)
                {
                    for (int p = bytesPerPixel; p < rowlength; p += 2)
                    {
                        int sub = ((actline[p] & 0xff) << 8) + (actline[p + 1] & 0xff);
                        int left = (((actline[p - bytesPerPixel] & 0xff) << 8)
                                + (actline[p - bytesPerPixel + 1] & 0xff));
                        actline[p] = (byte) (((sub + left) >> 8) & 0xff);
                        actline[p + 1] = (byte) ((sub + left) & 0xff);
                    }
                    break;
                }
                if (bitsPerComponent == 1 && colors == 1)
                {
                    // bytesPerPixel cannot be used:
                    // "A row shall occupy a whole number of bytes, rounded up if necessary.
                    // Samples and their components shall be packed into bytes
                    // from high-order to low-order bits."
                    for (int p = 0; p < rowlength; p++)
                    {
                        for (int bit = 7; bit >= 0; --bit)
                        {
                            int sub = (actline[p] >> bit) & 1;
                            if (p == 0 && bit == 7)
                            {
                                continue;
                            }
                            int left;
                            if (bit == 7)
                            {
                                // use bit #0 from previous byte
                                left = actline[p - 1] & 1;
                            }
                            else
                            {
                                // use "previous" bit
                                left = (actline[p] >> (bit + 1)) & 1;
                            }
                            if (((sub + left) & 1) == 0)
                            {
                                // reset bit
                                actline[p] = (byte) (actline[p] & ~(1 << bit));
                            }
                            else
                            {
                                // set bit
                                actline[p] = (byte) (actline[p] | (1 << bit));
                            }
                        }
                    }
                    break;
                }
                // everything else, i.e. bpc 2 and 4, but has been tested for bpc 1 and 8 too
                int elements = columns * colors;
                for (int p = colors; p < elements; ++p)
                {
                    int bytePosSub = p * bitsPerComponent / 8;
                    int bitPosSub = 8 - p * bitsPerComponent % 8 - bitsPerComponent;
                    int bytePosLeft = (p - colors) * bitsPerComponent / 8;
                    int bitPosLeft = 8 - (p - colors) * bitsPerComponent % 8 - bitsPerComponent;

                    int sub = getBitSeq(actline[bytePosSub], bitPosSub, bitsPerComponent);
                    int left = getBitSeq(actline[bytePosLeft], bitPosLeft, bitsPerComponent);
                    actline[bytePosSub] = (byte) calcSetBitSeq(actline[bytePosSub], bitPosSub, bitsPerComponent, sub + left);
                }
                break;
            case 10:
                // PRED NONE
                // do nothing
                break;
            case 11:
                // PRED SUB
                for (int p = bytesPerPixel; p < rowlength; p++)
                {
                    int sub = actline[p];
                    int left = actline[p - bytesPerPixel];
                    actline[p] = (byte) (sub + left);
                }
                break;
            case 12:
                // PRED UP
                for (int p = 0; p < rowlength; p++)
                {
                    int up = actline[p] & 0xff;
                    int prior = lastline[p] & 0xff;
                    actline[p] = (byte) ((up + prior) & 0xff);
                }
                break;
            case 13:
                // PRED AVG
                for (int p = 0; p < rowlength; p++)
                {
                    int avg = actline[p] & 0xff;
                    int left = p - bytesPerPixel >= 0 ? actline[p - bytesPerPixel] & 0xff : 0;
                    int up = lastline[p] & 0xff;
                    actline[p] = (byte) ((avg + (left + up) / 2) & 0xff);
                }
                break;
            case 14:
                // PRED PAETH
                for (int p = 0; p < rowlength; p++)
                {
                    int paeth = actline[p] & 0xff;
                    int a = p - bytesPerPixel >= 0 ? actline[p - bytesPerPixel] & 0xff : 0;// left
                    int b = lastline[p] & 0xff;// upper
                    int c = p - bytesPerPixel >= 0 ? lastline[p - bytesPerPixel] & 0xff : 0;// upperleft
                    int value = a + b - c;
                    int absa = Math.abs(value - a);
                    int absb = Math.abs(value - b);
                    int absc = Math.abs(value - c);

                    if (absa <= absb && absa <= absc)
                    {
                        actline[p] = (byte) ((paeth + a) & 0xff);
                    }
                    else if (absb <= absc)
                    {
                        actline[p] = (byte) ((paeth + b) & 0xff);
                    }
                    else
                    {
                        actline[p] = (byte) ((paeth + c) & 0xff);
                    }
                }
                break;
            default:
                break;
        }
    }

    public static void decodePredictor(int predictor, int colors, int bitsPerComponent, int columns, InputStream in, OutputStream out)
            throws IOException
    {
        if (predictor == 1)
        {
        }
        else
        {
            // calculate sizes
            final int rowlength = calculateRowLength(colors, bitsPerComponent, columns);
            byte[] actline = new byte[rowlength];
            byte[] lastline = new byte[rowlength];

            int linepredictor = predictor;

            while (in.available() > 0)
            {
                // test for PNG predictor; each value >= 10 (not only 15) indicates usage of PNG predictor
                if (predictor >= 10)
                {
                    // PNG predictor; each row starts with predictor type (0, 1, 2, 3, 4)
                    // read per line predictor
                    linepredictor = in.read();
                    if (linepredictor == -1)
                    {
                        return;
                    }
                    // add 10 to tread value 0 as 10, 1 as 11, ...
                    linepredictor += 10;
                }

                // read line
                int i, offset = 0;
                while (offset < rowlength && ((i = in.read(actline, offset, rowlength - offset)) != -1))
                {
                    offset += i;
                }

                decodePredictorRow(linepredictor, colors, bitsPerComponent, columns, actline, lastline);
                System.arraycopy(actline, 0, lastline, 0, rowlength);
                out.write(actline);
            }
        }
    }

    public static int calculateRowLength(int colors, int bitsPerComponent, int columns)
    {
        final int bitsPerPixel = colors * bitsPerComponent;
        return  (columns * bitsPerPixel + 7) / 8;
    }

    // get value from bit interval from a byte
    public static int getBitSeq(int by, int startBit, int bitSize)
    {
        int mask = ((1 << bitSize) - 1);
        return (by >>> startBit) & mask;
    }

    // set value in a bit interval and return that value
    public static int calcSetBitSeq(int by, int startBit, int bitSize, int val)
    {
        int mask = ((1 << bitSize) - 1);
        int truncatedVal = val & mask;
        mask = ~(mask << startBit);
        return (by & mask) | (truncatedVal << startBit);
    }
}
