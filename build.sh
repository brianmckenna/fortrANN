gfortran -c m_types.f90
gfortran -c m_constants.f90
gfortran -c m_file_util.f90
gfortran -c m_network.f90
gfortran -c m_epoch.f90
gfortran -c m_normalize.f90
gfortran -c m_moment.f90
gfortran -c m_data.f90
gfortran -c m_error.f90
gfortran -c m_levenberg_marquardt.f90
gfortran -c m_backprop.f90
gfortran -o ann_train ann_train.F90 *.o
gfortran -o ann_execute ann_execute.F90 *.o
