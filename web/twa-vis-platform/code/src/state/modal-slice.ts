import { createSlice } from '@reduxjs/toolkit';

import { ReduxState } from 'app/store';

export const modalSlice = createSlice({
  name: "modal",
  initialState: {
    isOpen: false,
  },
  reducers: {
    setIsOpen: (state, action) => {
      state.isOpen = action.payload;
    },
  }
})

// Export selectors 
export const getIsOpenState = (state: ReduxState) => state.modal.isOpen;

// Export the actions
export const { setIsOpen } = modalSlice.actions;

// Export the reducer
export default modalSlice.reducer;
