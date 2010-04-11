package uk.co.colinhowe.gleam.compiler;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class Speed {
  
  private static final int MACROS = 50;
  
  private static void createFile(int i, String s) throws IOException {
    File file = new File("performance/v" + i + ".gleam");
    FileWriter writer = new FileWriter(file);
    writer.write(s);
    writer.close();
  }

  private static void generateMacro(int i) throws IOException {
    String content = 
        "macro x" + i + " with s : string {\n"
     +  "  node p s\n"
     +  "}\n";
    createFile(i, content);
  }

  private static void generateView(int j) throws IOException {
    String content = 
        "x" + (j - MACROS) + " \"cats\"\n" 
     +  "node div {\n"
     +  "  node div {\n"
     +  "    node div {\n"
     +  "      node div {\n"
     +  "        node div {\n"
     +  "          node inner \"hoihoi\"\n"
     +  "        }\n"
     +  "      }\n"
     +  "    }\n"
     +  "  }\n"
     +  "}\n";
    createFile(j, content);
  }
  
  public static void main(String[] args) throws IOException {
    for (int i = 0; i < MACROS; i++) {
      generateMacro(i);
    }
    for (int j = MACROS; j < MACROS * 2; j++) {
      generateView(j);
    }
  }
}