#include <algorithm>
#include <cstddef>
#include <cassert>
#include <cmath>
#include <cstdlib>

#include <chrono>
#include <utility>

#include "list.h"
#include "math.h"
#include "output.h"

//TODO this class is temporary as fuck
namespace renderer
{
    class rasterizer
    {
public:
        math::matrix<DYNAMIC, DYNAMIC, output::RGBA32> raster;
        math::matrix<3> WtoSCR;

        rasterizer(size_t SCRwidth, size_t SCRheight, math::vector<2> worldXCoords, math::vector<2> worldYCoords) : 
        raster(output::RGBA32{255, 255, 255, 255}, SCRwidth, SCRheight)
        {
            const auto width = worldXCoords[1] - worldXCoords[0];
            const auto height = worldYCoords[1] - worldYCoords[0];
            const auto midX = worldXCoords.sum()/2;
            const auto midY = worldYCoords.sum()/2;

            WtoSCR = math::homogeneous<3>(math::matrix<2>::I, {0, 0, 1}, {-midX, -midY, 1});
            WtoSCR = WtoSCR * math::homogeneous<3>(math::scale<2>({float(SCRwidth)/(width),
            float(SCRheight)/(height)}), {0, 0, 1}, {0, 0, 1});
        }
        void rasterize(const math::vector<2, float>& worldLoc, const output::RGBA32& color)
        {
            //TODO this conversion is causing overflow.
            //TOO implement submatrix()
            math::vector<3, uint> pixelLoc = WtoSCR * math::vector<3, float>(join<float>(worldLoc, list({1.f})));
            
            if(pixelLoc.x > raster.cols() || pixelLoc.y > raster.rows())
                return;
            raster.element(pixelLoc.y, pixelLoc.x) = color;
        }

        math::matrix<DYNAMIC, DYNAMIC, output::RGBA32> get_framebuffer(const math::vector<2, uint>& renderTargetSize)
        {
            if(renderTargetSize.x == raster.cols() && renderTargetSize.y == raster.rows())
                return raster;

            math::matrix<DYNAMIC, DYNAMIC, output::RGBA32> renderTarget(renderTargetSize.y, renderTargetSize.x);
            //very slow, why? calling the function is the slowest part!
            //UPDATE : don't use std::function. ever. inline lambdas with template params.
            //https://stackoverflow.com/questions/67615330/why-stdfunction-is-too-slow-is-cpu-cant-utilize-instruction-reordering
            auto const ROWS = renderTarget.rows();
            auto const COLS = renderTarget.cols();
            for(size_t i = 0; i < ROWS ; ++i)
                for(size_t j = 0; j < COLS; ++j)
                {
                    constexpr float halfPixel = 0.5;
                    float vCenter = float(i + halfPixel)/renderTargetSize.y; //[0, 1)
                    float hCenter = float(j + halfPixel)/renderTargetSize.x;

                    renderTarget[i * renderTarget.cols() + j] = this->raster(vCenter * raster.rows(), hCenter * raster.cols());
                }
            return renderTarget;
        }
        
        //TODO make this work with all slopes
        void midpoint_line_draw(math::vec2f SCRp1, math::vec2f SCRp2, output::RGBA32 color = {0, 0, 0, 1}) //assume slope in [0, 1]
        {
            if(SCRp1.x > SCRp2.x)
                std::swap(SCRp1, SCRp2); //perhaps better to keep references
            
            const float& x0 = SCRp1.x;
            const float& y0 = SCRp1.y;
            const float& y1 = SCRp2.y;
            const float& x1 = SCRp2.x;

            float x = x0;
            float y = y0;

            while(x < SCRp2.x)
            {
                math::vec2f midpoint = {x + 1.f, y + 0.5f};
                float FxyAtMidpoint = (y0 - y1)*x + (x1 - x0)*y + x0*y1 - x1*y0;
                if(FxyAtMidpoint < 0) //midpoint is under the line
                    y++;
                raster.element(y, x) = color;
                x++;
            }
        }
        
    };
};
int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
        return -1;

    constexpr uint wFramebuffer = 100;
    constexpr uint hFramebuffer = 100;
    constexpr uint wWindow = 1000;
    constexpr uint hWindow = 1000;

    using namespace math;
    using namespace output;

    window window("title", 200, 200, wWindow, hWindow);

    const vector<2> Wx(0.f, 1.f);
    const vector<2> Wy(0.f, 1.f);
    
    struct ms_timer
    {
        std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();
        [[nodiscard]] std::chrono::milliseconds clock()
        {
            return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - start);
        }
    };
    
    

    renderer::rasterizer R(wFramebuffer, hFramebuffer, Wx, Wy);
    bool quit = false;
    ms_timer frameTimer;
    while(!quit)
    {
        SDL_Event e;
        while(SDL_PollEvent(&e))
            if(e.type == SDL_QUIT)
                quit = true;

        //R.rasterize(vec2f(0.f + 0.001f*float(frameTimer.clock().count()), 0.5f), RGBA32({255, 0, 0 , 255}));
        R.midpoint_line_draw({0, 0}, {50, 1});

        auto test = R.get_framebuffer(vec2u(wWindow, hWindow));
auto frameDelta = frameTimer.clock();
        window.write_frame(test);
    }
    return 0;
}
