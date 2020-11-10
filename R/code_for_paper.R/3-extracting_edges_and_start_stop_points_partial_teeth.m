%This code removes the edges of the black and white images as well as the start and stop points of the open curves created by the partially observed occlusal surface.  

id = "DSCN5514bw"
jpg = ".jpg"
file = id + jpg

uiopen("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images/"+file,1)

img = DSCN5514bw;

[n1,n2] = size(img);

img2 = img(:,:,1);
img2 = imbinarize(img2);
siz = size(img2)

figure(1), clf;
imagesc(img2);
colormap gray;
[x,y] = ginput(2);
%fix the way it stores the y values.
y = siz(1) - y

boundary = bwboundaries(img2);
boundary_out = boundary{2};
boundary_out(:,1) = siz(1) - boundary_out(:,1);
boundary_out = [boundary_out(:,2),boundary_out(:,1)]
plot(boundary_out(:,1), boundary_out(:,2));
hold on;
plot(x,y,"o");

csvwrite("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images_data/" + id + ".csv",boundary_out)


boundary_out_start_stop = [x,y]
csvwrite("/Users/gregorymatthews/Dropbox/shapeanalysisgit/partial_teeth/bw_images_data/" + id + "start_stop.csv",boundary_out_start_stop)



