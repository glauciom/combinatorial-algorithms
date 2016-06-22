package matrix;
/*
 * Created on 20/01/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class Element {
	
			private int line, column;
			double value;
		
			public Element(int newline, int newcolumn, double newvalue) {
				this.line = newline;
				this.column = newcolumn;
				this.value = newvalue;
			}
		
			public int getLine() {
				return this.line;
			}
		
			public int getColumn() {
				return this.column;
			}
		
			public double getValue() {
				return this.value;
			}
		
			public void setLine(int newline) {
				this.line = newline;
			}
		
			public void setColumn(int newcolumn) {
				this.column = newcolumn;
			}
		
			public void setValue(int newvalue){
				this.value = newvalue;
			}
		
			public boolean equals(Element e1) {
			  if (e1.getLine() == this.getLine() && e1.getColumn() == this.getColumn()) 
			   return true;
			 return false;
			}
			
			public String toString() {
				return ""+value;
			}
		}
