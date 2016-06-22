/*
 * Created on 25/07/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package statistics;

import kNSubset.RevolvingDoor;

/**
 * @author GLAUCIO
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class UnserialKNSubsetMethod {
	
	public int getSerialFromArray(int[] array) {
	//	LexicographicNextKSubset next = new LexicographicNextKSubset(60,6);
		RevolvingDoor next = new RevolvingDoor(60,6);
		int count = 1;
		while (!isEquals(array,next.getSubset())) {
			count++;
	//		next.lexicographicNextKSubsetAlgorithm();
			next.revolvingDoorAlgorithm();
		}
		return count;
	}
	
	private boolean isEquals(int[] array, int[] result) {
		for(int i = 0; i < array.length; i++)
			if(array[i] != result[i])
				return false;
	return true;
	}
}
