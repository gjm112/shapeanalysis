function [d,p2n] = GeodesicElasticClosed(p1,p2)

% input p1 and p2 as 2xn matrices; output is the distance d and the
% re-parameterized second curve p2n

figs=1;

p1 = ReSampleCurve(p1,200);
p2 = ReSampleCurve(p2,200);

if figs
figure(1); clf; hold on;
plot(p1(1,:),p1(2,:),'b','LineWidth',2);
plot(p2(1,:),p2(2,:),'r','LineWidth',2);
axis equal;
axis xy off;
end

q1 = curve_to_q(p1);
q2 = curve_to_q(p2);

tic
[q2n,R] = Find_Rotation_and_Seed_unique(q1,q2,1);
q2n = ProjectC(q2n);
p2n=q_to_curve(q2n);
p2n=R*p2n;
toc

d = sqrt(InnerProd_Q(q1-q2n,q1-q2n));