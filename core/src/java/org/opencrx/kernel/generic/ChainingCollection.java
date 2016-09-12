/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ChainingCollection
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of CRIXP Corp. nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.generic;

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * ChainingCollection
 *
 * @param <T>
 */
public class ChainingCollection<T> extends AbstractCollection<T> {

	/**
	 * ChainingIterator
	 *
	 */
	class ChainingIterator implements Iterator<T> {

		public ChainingIterator(
		) {
			this.index = 0;
			this.iterator = ChainingCollection.this.collections[0].iterator();
		}

		@Override
        public boolean hasNext(
        ) {
			boolean hasNext = this.iterator.hasNext();
			while(!hasNext && (this.index < ChainingCollection.this.collections.length - 1)) {
				this.index++;
				this.iterator = ChainingCollection.this.collections[this.index].iterator();
				hasNext = this.iterator.hasNext();
			}
			return hasNext;
        }

		@Override
        public T next(
        ) {
			return this.iterator.next();				
        }

		@Override
        public void remove(
        ) {
			throw new UnsupportedOperationException();
        }
		
		private Iterator<T> iterator;
		private int index;
	}
		
	/**
	 * Constructor.
	 * 
	 * @param collections
	 */
	public ChainingCollection(
		Collection<T>... collections
	) {
		this.collections = collections;
	}
	
	/**
	 * Constructor.
	 * 
	 * @param collections
	 */
	@SuppressWarnings("unchecked")
	public ChainingCollection(
		List<Collection<T>> collections
	) {
		this.collections = collections.toArray(new Collection[collections.size()]);
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#iterator()
	 */
	@Override
    public Iterator<T> iterator(
    ) {
		return new ChainingIterator();
    }

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
    public int size(
    ) {
		int size = 0;
		for(int i = 0; i < this.collections.length; i++) {
			size += this.collections[i].size();
		}
        return size;
    }

	protected final Collection<T>[] collections;
}

