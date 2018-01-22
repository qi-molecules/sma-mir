function pro_remove_dataset,fn

;A quick and dirty function that deletes the dataset.  It doesn't have to
;do much error checking as if the delete failed - it's because the file 
;is already gone - or it can't be deleted because of permission problems.
;Either way - there's nothing we can do!
;
;Written by Kim Drongesen
;November 18, 1998

;Define common blocks
common global

fn = e.dataset_dir+'/dataset/'+fn
result = fil_remove(fn)

return,result
end
