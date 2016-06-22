/*
 * Created on 01/03/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package matrix;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class LerBinario {

	private FileInputStream input;
	private FileOutputStream output; 
	private DataInputStream dis;
	private DataOutputStream out;
	File f,f1;
	
	public LerBinario() {
		lerEscreverBinario();
	}
	public void lerEscreverBinario() {
		try {
			f = new File("C://Documents and Settings//glaucio//Desktop//bkg_consultas1.jpg");
			input = new FileInputStream(f);
			dis = new DataInputStream(input);
			f1 = new File("C://Documents and Settings//glaucio//Desktop//teste.jpg");
			if (!f1.exists())
			 f1.createNewFile();
			output = new FileOutputStream(f1);
			out = new DataOutputStream(output);
		    byte[] buffer = new byte[(int)f.length()];
		    dis.read(buffer);
			String strBuffer = new String(buffer);
			int numbytestransf = 50;
			String strMsg = "";
			String concat = "";
			if (strBuffer != null){
				String strbytestransf;
				int i;			
				int teste = strBuffer.length();
				int teste2 = numbytestransf;
				for (i = 0; i < (strBuffer.length()/numbytestransf); i++){
					strbytestransf = strBuffer.substring(i*numbytestransf,(i+1)*numbytestransf);
					concat += strbytestransf;
				}
				}						
			out.write(buffer);	
		} catch (EOFException eof) {
			System.out.println("EOF reached ");
		} catch (IOException ioe) {
			System.out.println("IO error: " + ioe);
		}
	}
	
	public static void main(String[] args) {
	 new LerBinario();
	}
}
