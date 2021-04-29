package org.spbsu.scala;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public abstract class PrefixFilterInputStream extends InputStream {

    private final BufferedReader input;
    private final Charset encoding = StandardCharsets.UTF_8;
    private ByteArrayInputStream buffer;

    public PrefixFilterInputStream(InputStream is) throws IOException {
        input = new BufferedReader(new InputStreamReader(is, this.encoding));
        nextLine();
    }

    @Override
    public int read() throws IOException {
        if (buffer == null) {
            return -1;
        }
        int ch = buffer.read();
        if (ch == -1) {
            if (!nextLine()) {
                return -1;
            }
            return read();
        }
        return ch;
    }

    private boolean nextLine() throws IOException {
        String line;
        while ((line = input.readLine()) != null) {
            if (!excludeLine(line)) {
                line += '\n';
                buffer = new ByteArrayInputStream(line.getBytes(encoding));
                return true;
            }
        }
        return false;
    }

    @Override
    public void close() throws IOException {
        input.close();
    }

    abstract boolean excludeLine(String line);

}