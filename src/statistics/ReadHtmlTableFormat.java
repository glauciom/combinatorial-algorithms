/*
 * Created on 25/07/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package statistics;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.StringTokenizer;

/**
 * @author GLAUCIO
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ReadHtmlTableFormat {

	private int[][] data;
	private int lines;
	
	public void readData(File f, File des, int dezenas) {
		String s = "", out = "";
		try {
			BufferedReader reader = new BufferedReader(new FileReader(f));
			while ((s = reader.readLine()) != null) {
				out += s + "\n";	
			}
			reader.close();
			stractData(out, des, dezenas);
		} catch (Exception e) {
			// TODO: handle exception
			e.printStackTrace();
		}
		
	}
	
	private void stractData(String src, File des, int dezenas) throws IOException {
		StringTokenizer token = new StringTokenizer(src, "\r \n");
		String actual = "";
		String out = "";
		while (token.hasMoreTokens()) {
			actual = token.nextToken();
			if (actual.trim().toLowerCase().startsWith("<tr")) {
				lines++;
				String s = token.nextToken();
				if (s.trim().startsWith("bgcolor")) {
					out += getInner(token.nextToken()," -> ");	
				} else
					out += getInner(s," -> ");
				token.nextToken();
				for(int i = 0; i < dezenas; i++) {
					out += getInner(token.nextToken()," ");
				}
				out += "\n";
			}
		}
		copyData(out,dezenas);
		String s = "";
		for (int i = 0; i < data.length; i++) {
			UnserialKNSubsetMethod test = new UnserialKNSubsetMethod();
			data[i] = sort(data[i]);
			System.out.println("i = "+i);
			for (int j = 0; j < data[i].length; j++) {
				System.out.print(data[i][j] + " ");
			}
			System.out.println();
			int res = test.getSerialFromArray(data[i]);
			s += (i+1) + " -> "+ res+ "\n";
	//		System.out.println(res);
		}
		System.out.println(s);
		BufferedWriter writer = new BufferedWriter(new FileWriter(des));
		writer.write(s);
		writer.close();
		
	}
	
	public int[] sort(int v[]) {
	 int temp, aux;
	 for(int i = 0; i < v.length-1; i++) {
	  temp = i;
	  for(int j=i+1; j < v.length; j++)
	   if(v[j] < v[temp])
		temp=j;
	  aux = v[i];
	  v[i] = v[temp];
	  v[temp] = aux;
	 }
	 return v;
	}
	private void copyData(String out, int dezenas) {
		data = new int[lines][dezenas];
		StringTokenizer token = new StringTokenizer(out," \n");
		int i = 0, j = 0;
		while(token.hasMoreTokens()) {
			token.nextToken();
			token.nextToken();
			for(j = 0; j < dezenas; j++) {
				data[i][j] = Integer.parseInt(token.nextToken());
			}
			i++;
		}
	/*	for (i = 0; i < data.length; i++) {
			for (j = 0; j < data[i].length; j++) {
				System.out.print(data[i][j] + " ");
			}
			System.out.println();
		}*/
	}
	
	public void readGlaFormat(File f) {
		String s = "", out = "";
		try {
			BufferedReader reader = new BufferedReader(new FileReader(f));
			lines = 1;
			while ((s = reader.readLine()) != null) {
				StringTokenizer token = new StringTokenizer(s);
				token.nextToken();
				token.nextToken();
				out +=  token.nextToken()+ "\n";
				lines++;	
			}
			int[] values = new int[lines-1];
			StringTokenizer token = new StringTokenizer(out,"\n");
			for (int i = 0; i < values.length; i++) {
				values[i] = Integer.parseInt(token.nextToken());
			}
	//		values = sort(values);
			for (int i = 0; i < values.length; i++) {
				//if ((i+1) % 10 != 0)
					System.out.println(values[i]);
				//else
				//	System.out.println(values[i]+"\t");
			}
		//	System.out.println(out);
			reader.close();
		} catch (Exception e) {
			// TODO: handle exception
			e.printStackTrace();
		}
	}
	private String getInner(String str, String separator) {
		String res = str.replaceAll("<td>","");
		return res.replaceAll("</td>","") + separator;
	}	
	
	public static void main(String[] args) {
		ReadHtmlTableFormat test = new ReadHtmlTableFormat();
	//	test.readData(new File("./D_MEGA.HTM"),new File("./working1.gla"),6);
	 	test.readGlaFormat(new File("./working1.gla"));
		
	}
}
